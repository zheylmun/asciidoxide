use zed_extension_api as zed;

struct AsciidocLsp;

impl zed::Extension for AsciidocLsp {
    fn new() -> Self
    where
        Self: Sized,
    {
        AsciidocLsp
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let path = worktree
            .which("asciidoxide-lsp")
            .ok_or_else(|| "asciidoxide-lsp not found in PATH".to_string())?;

        Ok(zed::Command::new(path))
    }
}

zed::register_extension!(AsciidocLsp);
