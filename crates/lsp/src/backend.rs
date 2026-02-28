use std::time::Instant;

use crate::document::DocumentState;
use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRange, FoldingRangeParams,
    FoldingRangeProviderCapability, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    Location, MessageType, OneOf, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer};

pub struct Backend {
    client: Client,
    document_map: DashMap<String, DocumentState>,
}

impl Backend {
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: DashMap::new(),
        }
    }

    async fn log(&self, msg: impl std::fmt::Display) {
        self.client.log_message(MessageType::LOG, msg).await;
    }

    async fn publish_diagnostics(&self, uri: &str, state: &DocumentState) {
        let lsp_diagnostics = state.lsp_diagnostics();
        let count = lsp_diagnostics.len();
        if let Ok(uri) = Url::parse(uri) {
            self.client
                .publish_diagnostics(uri, lsp_diagnostics, Some(state.version()))
                .await;
        }
        self.log(format!("published {count} diagnostics")).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.log("shutdown requested").await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let version = params.text_document.version;
        let text = params.text_document.text;
        let len = text.len();
        self.log(format!("didOpen {uri} v{version} ({len} bytes)"))
            .await;

        let start = Instant::now();
        let state = DocumentState::new(version, text);
        let elapsed = start.elapsed();
        self.log(format!("parsed in {elapsed:?}")).await;

        self.document_map.insert(uri.clone(), state);

        if let Some(entry) = self.document_map.get(&uri) {
            self.publish_diagnostics(&uri, &entry).await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let version = params.text_document.version;
        self.log(format!("didChange {uri} v{version}")).await;

        if let Some(change) = params.content_changes.into_iter().last() {
            let len = change.text.len();
            let start = Instant::now();
            if let Some(mut entry) = self.document_map.get_mut(&uri) {
                entry.reparse(version, change.text);
            } else {
                self.document_map
                    .insert(uri.clone(), DocumentState::new(version, change.text));
            }
            let elapsed = start.elapsed();
            self.log(format!("reparsed {len} bytes in {elapsed:?}"))
                .await;

            if let Some(entry) = self.document_map.get(&uri) {
                self.publish_diagnostics(&uri, &entry).await;
            }
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri.to_string();
        self.log(format!("documentSymbol {uri}")).await;

        let symbols = self
            .document_map
            .get(&uri)
            .map(|entry| entry.document_symbols())
            .unwrap_or_default();
        self.log(format!("returning {} symbols", symbols.len()))
            .await;
        Ok(Some(DocumentSymbolResponse::Nested(symbols)))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri.to_string();
        self.log(format!("foldingRange {uri}")).await;

        let ranges = self
            .document_map
            .get(&uri)
            .map(|entry| entry.folding_ranges())
            .unwrap_or_default();
        self.log(format!("returning {} folding ranges", ranges.len()))
            .await;
        Ok(Some(ranges))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let uri_str = uri.to_string();
        self.log(format!(
            "gotoDefinition {uri_str} {}:{}",
            position.line, position.character
        ))
        .await;

        let range = self
            .document_map
            .get(&uri_str)
            .and_then(|entry| entry.goto_definition(position));
        self.log(if range.is_some() {
            "definition found"
        } else {
            "definition not found"
        })
        .await;
        Ok(range.map(|r| GotoDefinitionResponse::Scalar(Location { uri, range: r })))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;
        self.log(format!(
            "hover {uri} {}:{}",
            position.line, position.character
        ))
        .await;

        let result = self
            .document_map
            .get(&uri)
            .and_then(|entry| entry.hover(position));
        self.log(format!(
            "hover {}",
            if result.is_some() {
                "content returned"
            } else {
                "no content"
            }
        ))
        .await;
        Ok(result)
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.log(format!("didClose {uri}")).await;
        self.document_map.remove(&uri);

        // Clear diagnostics for the closed document
        if let Ok(uri) = Url::parse(&uri) {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }
}
