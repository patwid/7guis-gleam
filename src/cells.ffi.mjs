export function focus_cell_editor() {
  requestAnimationFrame(() => {
    const el = document.getElementById("cell-editor");
    if (el) { el.focus(); el.select(); }
  });
}
