#!/usr/bin/env python3
"""Pre-process Oxford Academic HTML so that Auto-CORPus can extract per-section labels.

Background
----------
Oxford Academic (Silverchair) article HTML places every section heading as a
*flat sibling* of the paragraph content inside a single container:

    <div class="widget-items" data-widgetname="ArticleFulltext">
      <h2 class="section-title">Introduction</h2>
      <p class="chapter-para">...</p>
      <h2 class="section-title">Methods</h2>
      <h3 class="section-title">Study design</h3>
      <p class="chapter-para">...</p>
      ...
    </div>

Auto-CORPus's section logic expects a per-section wrapper element whose
contents are the heading plus that section's paragraphs. On this flat layout
it can only find one "section" (the whole body), and every paragraph ends up
labelled with whatever ``headers[0]`` happened to be — most often
"Author notes", because of a reset bug in ``handle_not_tables`` that keeps
only the last matching header definition.

This script re-nests the HTML so that each h2 and the siblings that follow it
(up to the next h2) are wrapped in ``<div class="ac-section">``. With that
shape, Auto-CORPus matches one section per wrapper and populates the correct
heading. The existing h3/h4/h5 ``previous_sibling`` logic in Auto-CORPus
continues to handle subsections inside each wrapper.

Paired config change
--------------------
After running this script, update ``config_oxford_academic.json`` so that the
``sections`` block is defined by the new wrapper:

    "sections": {
        "data": {
            "headers": [
                {
                    "tag": "h2",
                    "attrs": {
                        "class": "(?:section-title|abstract-title|backreferences-title|backacknowledgements-title|backnotes-title|backsection-title|authorNotes-section-title)"
                    }
                }
            ]
        },
        "defined-by": [
            {"tag": "div", "attrs": {"class": "ac-section"}}
        ]
    }

Usage
-----
    python preprocess_oxford_html.py \\
        --input  output/fulltexts/oxford_academic/html \\
        --output output/fulltexts/oxford_academic/html_ac_ready

Then run Auto-CORPus against ``html_ac_ready`` instead of ``html``:

    auto-corpus -b OXFORD_ACADEMIC \\
                -t output/fulltexts/oxford_academic \\
                -f output/fulltexts/oxford_academic/html_ac_ready \\
                -o XML
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

from bs4 import BeautifulSoup, NavigableString, Tag

logger = logging.getLogger("preprocess_oxford_html")

# ---------------------------------------------------------------------------
# Configuration: which widget containers hold flat h2-delimited sections, and
# which h2 classes mark a section boundary.
# ---------------------------------------------------------------------------

# data-widgetname values whose direct children we should re-nest. We include
# both the main article text and the supplementary-data widget, since the
# "Supplementary data" h2 lives in a different widget on some articles.
SECTION_WIDGET_NAMES = {
    "ArticleFulltext",
    "OUP_Data_Supplements_Tab",
}

# Any h2 with one of these classes starts a new section. We match on any class
# token (Silverchair stacks classes like "section-title js-splitscreen-...").
SECTION_HEADING_CLASSES = {
    "section-title",
    "abstract-title",
    "backreferences-title",
    "backacknowledgements-title",
    "backnotes-title",
    "backsection-title",
    "authorNotes-section-title",
}

# Heading classes that mark the article's Abstract. We split the abstract run
# at the first top-level body paragraph so brief-communication articles (which
# omit body h2 dividers and put body content directly between Abstract and
# Funding/References) don't end up with all body text mislabelled "Abstract".
ABSTRACT_HEADING_CLASSES = {
    "abstract-title",
}

# Marker class put on the wrappers we create. Used by the companion config
# change and by this script itself (for idempotence).
WRAPPER_CLASS = "ac-section"


# ---------------------------------------------------------------------------
# Core transformation
# ---------------------------------------------------------------------------

def _is_section_heading(tag: Tag) -> bool:
    """Return True if ``tag`` is an h2 that starts a new section."""
    if not isinstance(tag, Tag) or tag.name != "h2":
        return False
    classes = tag.get("class") or []
    return any(c in SECTION_HEADING_CLASSES for c in classes)


def _is_abstract_heading(tag: Tag) -> bool:
    """Return True if ``tag`` is the Abstract h2 (vs another section h2)."""
    if not isinstance(tag, Tag) or tag.name != "h2":
        return False
    classes = tag.get("class") or []
    return any(c in ABSTRACT_HEADING_CLASSES for c in classes)


def _is_top_level_body_paragraph(node) -> bool:
    """Return True if ``node`` is a chapter-para sibling (body content).

    Used to detect that an Abstract run has bled into article body content.
    Brief-communication Oxford articles place body paragraphs as direct
    siblings of the Abstract heading, so we split there to avoid mislabelling
    them.
    """
    if not isinstance(node, Tag) or node.name != "p":
        return False
    classes = node.get("class") or []
    return "chapter-para" in classes


def _split_abstract_run(run_nodes: list) -> tuple[list, list]:
    """Split an Abstract run at the first top-level body paragraph.

    Returns ``(abstract_run, body_run)``. ``body_run`` is empty for normal
    articles (no top-level chapter-para nodes after the Abstract h2) and
    contains the body content for brief-communication articles that lack a
    body-level h2 divider.
    """
    for i, node in enumerate(run_nodes):
        if _is_top_level_body_paragraph(node):
            return run_nodes[:i], run_nodes[i:]
    return run_nodes, []


def _already_wrapped(container: Tag) -> bool:
    """Return True if the container's children already look pre-processed.

    Used so re-running the script on an output directory is a no-op.
    """
    for child in container.find_all(recursive=False):
        classes = child.get("class") or []
        if WRAPPER_CLASS in classes:
            return True
    return False


def wrap_sections_in_container(container: Tag, soup: BeautifulSoup) -> int:
    """Re-nest ``container``'s flat h2-delimited children into wrapper divs.

    Each h2 that matches ``SECTION_HEADING_CLASSES`` begins a new wrapper. All
    following siblings (tags, whitespace, navigable strings) are moved into
    that wrapper until the next such h2.

    Content that appears *before* the first h2 (e.g. the abstract's own
    ``<section>`` element, or a ``<div class="kwd-group">``) is left in place
    at the container's top level so the existing Auto-CORPus rules for
    abstracts and keywords still match it.

    Args:
        container: The widget-level div whose children should be re-nested.
        soup: The parent BeautifulSoup document, used to create new tags.

    Returns:
        The number of ``ac-section`` wrappers created inside ``container``.
    """
    if _already_wrapped(container):
        return 0

    # Materialise the child list first — we'll be mutating the tree.
    original_children = list(container.children)

    # Collect runs: each run is (heading_tag_or_None, [following_nodes]).
    runs: list[tuple[Tag | None, list]] = []
    current_heading: Tag | None = None
    current_run: list = []
    preface: list = []

    for node in original_children:
        if isinstance(node, Tag) and _is_section_heading(node):
            # Close out any in-flight run.
            if current_heading is not None:
                runs.append((current_heading, current_run))
            current_heading = node
            current_run = []
        else:
            if current_heading is None:
                preface.append(node)
            else:
                current_run.append(node)

    if current_heading is not None:
        runs.append((current_heading, current_run))

    if not runs:
        return 0  # No h2 headings in this container — nothing to do.

    # Detach every node from the container so we can rebuild the order cleanly.
    for node in original_children:
        if hasattr(node, "extract"):
            node.extract()

    # Re-append the preface content unchanged (abstract section, keywords,
    # any stray whitespace before the first h2).
    for node in preface:
        container.append(node)

    # For each run, build a wrapper that owns the h2 and its following nodes.
    # Special case: an Abstract run with top-level body paragraphs (brief
    # communications) is split into two wrappers so the body doesn't inherit
    # the "Abstract" heading.
    wrappers_created = 0
    for heading, run_nodes in runs:
        if _is_abstract_heading(heading):
            abstract_nodes, body_nodes = _split_abstract_run(run_nodes)
        else:
            abstract_nodes, body_nodes = run_nodes, []

        wrapper = soup.new_tag("div")
        wrapper["class"] = [WRAPPER_CLASS]
        wrapper.append(heading)
        for node in abstract_nodes:
            wrapper.append(node)
        container.append(wrapper)
        wrappers_created += 1

        if body_nodes:
            # Heading-less wrapper. Auto-CORPus will fall back to labelling
            # this section "document part" via _set_unknown_section_headings.
            body_wrapper = soup.new_tag("div")
            body_wrapper["class"] = [WRAPPER_CLASS]
            for node in body_nodes:
                body_wrapper.append(node)
            container.append(body_wrapper)
            wrappers_created += 1

    return wrappers_created


def preprocess_soup(soup: BeautifulSoup) -> dict[str, int]:
    """Wrap sections in every relevant widget container found in ``soup``.

    Returns a small summary dict: ``{widget_name: wrappers_added}``. Useful
    for logging and for failing loudly when a file looks malformed.
    """
    summary: dict[str, int] = {}
    for container in soup.find_all("div", attrs={"data-widgetname": True}):
        widget_name = container.get("data-widgetname")
        if widget_name not in SECTION_WIDGET_NAMES:
            continue
        added = wrap_sections_in_container(container, soup)
        summary[widget_name] = summary.get(widget_name, 0) + added
    return summary


# ---------------------------------------------------------------------------
# File-level driver
# ---------------------------------------------------------------------------

def process_file(src: Path, dst: Path) -> dict[str, int]:
    """Read ``src``, re-nest section boundaries, write to ``dst``.

    Returns the per-widget wrapper-count summary. Writes nothing if the file
    has no recognised section widgets (but logs a warning).
    """
    html = src.read_text(encoding="utf-8")
    soup = BeautifulSoup(html, "html.parser")

    summary = preprocess_soup(soup)
    total = sum(summary.values())

    if total == 0:
        logger.warning(
            "no section widgets wrapped in %s "
            "(no <div data-widgetname=ArticleFulltext> with h2 headings found)",
            src.name,
        )

    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_text(str(soup), encoding="utf-8")
    return summary


def process_directory(input_dir: Path, output_dir: Path) -> None:
    """Process every ``*.html`` file under ``input_dir`` into ``output_dir``."""
    html_files = sorted(input_dir.glob("*.html"))
    if not html_files:
        logger.error("no .html files found in %s", input_dir)
        sys.exit(1)

    logger.info("processing %d file(s) from %s", len(html_files), input_dir)

    total_files = 0
    total_sections = 0
    for src in html_files:
        dst = output_dir / src.name
        summary = process_file(src, dst)
        wrappers = sum(summary.values())
        total_files += 1
        total_sections += wrappers
        logger.info(
            "  %s -> %s  (%d ac-section wrapper(s): %s)",
            src.name,
            dst.name,
            wrappers,
            ", ".join(f"{k}={v}" for k, v in summary.items()) or "none",
        )

    logger.info(
        "done: %d file(s), %d total ac-section wrappers",
        total_files,
        total_sections,
    )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=(
            "Re-nest Oxford Academic HTML into per-section wrappers so "
            "Auto-CORPus can extract correct section labels."
        )
    )
    p.add_argument(
        "--input",
        required=True,
        type=Path,
        help="Directory of original Oxford Academic .html files.",
    )
    p.add_argument(
        "--output",
        required=True,
        type=Path,
        help="Directory to write pre-processed HTML into.",
    )
    p.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Emit DEBUG-level logs.",
    )
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s: %(message)s",
    )

    if not args.input.is_dir():
        logger.error("input path is not a directory: %s", args.input)
        return 2
    if args.output.resolve() == args.input.resolve():
        logger.error("output directory must differ from input directory")
        return 2

    process_directory(args.input, args.output)
    return 0


if __name__ == "__main__":
    sys.exit(main())
