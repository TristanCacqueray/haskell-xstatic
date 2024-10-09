// A simple script to export the core features

import { EditorView, basicSetup } from "codemirror";
import { ViewPlugin } from "@codemirror/view";
import { EditorState, ChangeSet } from "@codemirror/state";
import { markdown } from "@codemirror/lang-markdown";

globalThis.CodeMirror = {
  EditorView,
  ViewPlugin,
  EditorState,
  ChangeSet,
  basicSetup,
  markdown,
};
