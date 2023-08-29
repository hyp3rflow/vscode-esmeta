import * as path from "path";
import { ExtensionContext } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const module = {
    module: context.asAbsolutePath(path.join("server", "dist", "server.js")),
    transport: TransportKind.ipc,
  };
  const serverOptions: ServerOptions = { run: module, debug: module };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "ires" }],
    diagnosticCollectionName: "esmeta",
  };
  client = new LanguageClient(
    "esmeta-language-server",
    "ESMeta Language Server",
    serverOptions,
    clientOptions,
  );
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (client) return client.stop();
}
