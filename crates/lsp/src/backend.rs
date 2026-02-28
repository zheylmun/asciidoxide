use crate::document::DocumentState;
use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, InitializedParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
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

    async fn publish_diagnostics(&self, uri: &str, state: &DocumentState) {
        let lsp_diagnostics = state.lsp_diagnostics();
        if let Ok(uri) = Url::parse(uri) {
            self.client
                .publish_diagnostics(uri, lsp_diagnostics, Some(state.version()))
                .await;
        }
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
                ..ServerCapabilities::default()
            },
            ..InitializeResult::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(
                tower_lsp::lsp_types::MessageType::INFO,
                "server initialized",
            )
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let version = params.text_document.version;
        let text = params.text_document.text;
        let state = DocumentState::new(version, text);
        self.document_map.insert(uri.clone(), state);

        if let Some(entry) = self.document_map.get(&uri) {
            self.publish_diagnostics(&uri, &entry).await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let version = params.text_document.version;
        if let Some(change) = params.content_changes.into_iter().last() {
            if let Some(mut entry) = self.document_map.get_mut(&uri) {
                entry.reparse(version, change.text);
            } else {
                self.document_map
                    .insert(uri.clone(), DocumentState::new(version, change.text));
            }

            if let Some(entry) = self.document_map.get(&uri) {
                self.publish_diagnostics(&uri, &entry).await;
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.document_map.remove(&uri);

        // Clear diagnostics for the closed document
        if let Ok(uri) = Url::parse(&uri) {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }
    }
}
