# Split one long line into chunks of at most `max` chars, breaking only at
# sentence boundaries (". " followed by a capital) so no pair is cut in half.
{
    text = $0
    while (length(text) > max) {
        cut = 0
        for (i = max; i > 1; i--) {
            if (substr(text, i, 2) == ". " && substr(text, i + 2, 1) ~ /[A-Z0-9]/) { cut = i; break }
        }
        if (cut == 0) {                      # no boundary found: fall back to a space
            for (i = max; i > 1; i--) if (substr(text, i, 1) == " ") { cut = i - 1; break }
        }
        if (cut == 0) cut = max              # give up, hard split
        print substr(text, 1, cut)
        text = substr(text, cut + 2)
    }
    if (length(text)) print text
}
