#!/usr/bin/env python3
"""Pre-process AACR journal HTML so that Auto-CORPus can extract per-section labels.

Covers journals hosted on aacrjournals.org (Cancer Research, Clinical Cancer
Research, Cancer Epidemiology Biomarkers & Prevention, etc.).

Background
----------
AACR moved to Silverchair (the same platform as Oxford Academic), so article
HTML has the same *flat* layout: every heading is a sibling of the content
that follows it inside one ``<div class="widget-items"
data-widgetname="ArticleFulltext">`` container::

    <h2 class="abstract-title">Abstract</h2>
    <div><section class="abstract"><p>...</p></section></div>
    <div><div class="boxed-text"><div class="caption">
        <h3 class="title">Translational Relevance</h3></div><p>...</p></div></div>
    <h2 class="section-title">Materials and Methods</h2>
    <div></div>
    <h3 class="section-title">Study population</h3>
    <div><p>...</p><p>...</p></div>
    ...
    <h2 class="backreferences-title">References</h2>
    <div><div class="ref-list"> ... <div class="ref-content">...</div> ...

Differences from Oxford Academic that this script (and ``config_aacr.json``)
handle:

* Body paragraphs are plain ``<p>`` inside a plain ``<div>`` (no
  ``chapter-para`` class), so Auto-CORPus's "h3 immediately before the <p>"
  sub-heading rule never fires. We therefore wrap each h3 run in
  ``<div class="ac-subsection">`` as well as each h2 run in
  ``<div class="ac-section">``.
* Clinical Cancer Research articles carry a "Translational Relevance" box
  (``div.boxed-text``) straight after the abstract. We lift it into its own
  ``ac-section`` so it is not labelled as part of the Abstract.
* Reference items are ``div.ref-content`` (not ``js-splitview-ref-item``).
  Their ``citation-links`` panels (Google Scholar / Crossref / PubMed) are
  stripped so the BioC reference text is clean.

Usage
-----
    python preprocess_aacr_html.py \\
        --input  output/fulltexts/aacr/html \\
        --output output/fulltexts/aacr/html_ac_ready

    auto-corpus -b AACR \\
                -t output/fulltexts/aacr \\
                -f output/fulltexts/aacr/html_ac_ready \\
                -o XML
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

from bs4 import BeautifulSoup, Tag

logger = logging.getLogger("preprocess_aacr_html")

SECTION_WIDGET_NAMES = {"ArticleFulltext"}

# h2 classes that start a new top-level section (matched on any class token).
SECTION_HEADING_CLASSES = {
    "section-title",
    "abstract-title",
    "backreferences-title",
    "backacknowledgements-title",
    "backnotes-title",
    "backsection-title",
}
# h3 classes that start a sub-section inside a section.
SUBSECTION_HEADING_CLASSES = {"section-title"}

SECTION_WRAPPER_CLASS = "ac-section"
SUBSECTION_WRAPPER_CLASS = "ac-subsection"

REF_ITEM_CLASS = "ref-content"
REF_JUNK_CLASSES = {"citation-links", "refLink-parent"}


def _has_class(tag, name: str, classes: set[str]) -> bool:
    return (
        isinstance(tag, Tag)
        and tag.name == name
        and any(c in classes for c in (tag.get("class") or []))
    )


def _wrap_runs(container: Tag, soup: BeautifulSoup, is_heading, wrapper_class: str) -> int:
    """Wrap each heading in ``container`` plus its following siblings in a div.

    Nodes before the first heading are left in place. Returns wrappers created.
    """
    children = list(container.children)
    if not any(is_heading(c) for c in children):
        return 0

    preface, runs = [], []
    for node in children:
        if is_heading(node):
            runs.append([node])
        elif runs:
            runs[-1].append(node)
        else:
            preface.append(node)

    for node in children:
        node.extract()
    for node in preface:
        container.append(node)
    for run in runs:
        wrapper = soup.new_tag("div", attrs={"class": wrapper_class})
        for node in run:
            wrapper.append(node)
        container.append(wrapper)
    return len(runs)


def _lift_boxed_text(section: Tag, soup: BeautifulSoup) -> int:
    """Move boxed-text panels (e.g. Translational Relevance) into own sections.

    The new section is inserted immediately after ``section`` and gets an h2
    built from the box's caption title so Auto-CORPus labels it correctly.
    """
    lifted = 0
    anchor = section
    for box in section.find_all("div", class_="boxed-text"):
        title_tag = box.find(["h3", "h4"], class_="title")
        title = title_tag.get_text(" ", strip=True) if title_tag else "Boxed text"
        caption = title_tag.find_parent("div", class_="caption") if title_tag else None
        if caption is not None:
            caption.decompose()
        elif title_tag is not None:
            title_tag.decompose()

        new_sec = soup.new_tag("div", attrs={"class": SECTION_WRAPPER_CLASS})
        h2 = soup.new_tag("h2", attrs={"class": "section-title"})
        h2.string = title
        new_sec.append(h2)
        new_sec.append(box.extract())
        anchor.insert_after(new_sec)
        anchor = new_sec
        lifted += 1
    return lifted


def strip_reference_junk(soup: BeautifulSoup) -> int:
    removed = 0
    for ref_item in soup.find_all("div", class_=REF_ITEM_CLASS):
        for junk_cls in REF_JUNK_CLASSES:
            for junk in ref_item.find_all(class_=junk_cls):
                junk.decompose()
                removed += 1
    return removed


def preprocess_soup(soup: BeautifulSoup) -> dict[str, int]:
    summary = {"sections": 0, "subsections": 0, "boxed_text": 0}
    for container in soup.find_all("div", attrs={"data-widgetname": True}):
        if container.get("data-widgetname") not in SECTION_WIDGET_NAMES:
            continue
        # Idempotence: skip containers we have already processed.
        if container.find("div", class_=SECTION_WRAPPER_CLASS, recursive=False):
            continue
        summary["sections"] += _wrap_runs(
            container, soup,
            lambda n: _has_class(n, "h2", SECTION_HEADING_CLASSES),
            SECTION_WRAPPER_CLASS,
        )
        for section in container.find_all("div", class_=SECTION_WRAPPER_CLASS, recursive=False):
            summary["subsections"] += _wrap_runs(
                section, soup,
                lambda n: _has_class(n, "h3", SUBSECTION_HEADING_CLASSES),
                SUBSECTION_WRAPPER_CLASS,
            )
            h2 = section.find("h2", recursive=False)
            if h2 is not None and "abstract-title" in (h2.get("class") or []):
                summary["boxed_text"] += _lift_boxed_text(section, soup)
    summary["refs_cleaned"] = strip_reference_junk(soup)
    return summary


def process_file(src: Path, dst: Path) -> dict[str, int]:
    soup = BeautifulSoup(src.read_text(encoding="utf-8"), "html.parser")
    summary = preprocess_soup(soup)
    if summary["sections"] == 0:
        logger.warning("no ArticleFulltext sections wrapped in %s", src.name)
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_text(str(soup), encoding="utf-8")
    return summary


def process_directory(input_dir: Path, output_dir: Path) -> None:
    html_files = sorted(input_dir.glob("*.html"))
    if not html_files:
        logger.error("no .html files found in %s", input_dir)
        sys.exit(1)
    logger.info("processing %d file(s) from %s", len(html_files), input_dir)
    for src in html_files:
        summary = process_file(src, output_dir / src.name)
        logger.info("  %s  %s", src.name, ", ".join(f"{k}={v}" for k, v in summary.items()))
    logger.info("done: %d file(s) written to %s", len(html_files), output_dir)


def main(argv: list[str] | None = None) -> None:
    p = argparse.ArgumentParser(
        description="Re-nest AACR (Silverchair) HTML into per-section wrappers for Auto-CORPus."
    )
    p.add_argument("--input", required=True, type=Path, help="Directory of AACR .html files.")
    p.add_argument("--output", required=True, type=Path, help="Directory for pre-processed HTML.")
    p.add_argument("-v", "--verbose", action="store_true")
    args = p.parse_args(argv)
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(message)s",
    )
    process_directory(args.input, args.output)


if __name__ == "__main__":
    main()
