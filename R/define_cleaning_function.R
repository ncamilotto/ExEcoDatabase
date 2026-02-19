#' @title define_cleaning_function
#'
#'
#' @description 
#' Performs an extensive cleaning of text data, specifically targeting artifacts 
#' from PDF conversions, OCR processes, and LaTeX math formatting.
#' 
#' The function executes a multi-step pipeline:
#' 1. **HTML & Structure**: Removes HTML tags and fixes newline encoding.
#' 2. **LaTeX Conversion**: Translates complex LaTeX math expressions (exponents, 
#'    indices, `\mathrm`) into readable plain text and removes command syntax.
#' 3. **OCR Repair**: Corrects common Optical Character Recognition errors 
#'    (e.g., confusion between 'cl'/'d', 'O'/'0', 'I'/'l').
#' 4. **Typography**: Normalizes quotes, dashes, and spacing, with specific 
#'    rules optimized for French text (dates, apostrophes).
#'
#' @param text A character vector containing the raw text to be cleaned.
#'
#' @return A character vector of the same length with sanitized content.
#'
#' @export

clean_markdown <- function(text) {
  
  # 1. Remove HTML tags
  text <- str_replace_all(text, "<[^>]+>", "")
  
  # 2. Replace literal "\\n" with actual newlines
  text <- str_replace_all(text, "\\\\n", "\n")
  
  # 3. Handle mathematical expressions Before removing general tags
  
  # 3.1. ${ }^{\text {te }}$ -> te (empty brace structure)
  text <- str_replace_all(text, "\\$\\{\\s*\\}\\^\\{\\\\text\\s*\\{\\s*([^{}]*)\\s*\\}\\}\\$", "\\1")
  
  # 3.2 ${ }^{67}$ -> 67 (empty brace with numeric exponent)
  text <- str_replace_all(text, "\\$\\{\\s*\\}\\^\\{([0-9]+)\\}\\$", "\\1")
  
  # 3.3. $\mathrm{M}^{\mathrm{r}}$ -> Mr
  text <- str_replace_all(text, "\\$\\\\mathrm\\{([^}]+)\\}\\^\\{\\\\mathrm\\{([^}]+)\\}\\}\\$", "\\1\\2")
  
  # 3.4. $\mathrm{n}^{\circ} 4^{8}$ -> n°48 (with exponent)
  text <- str_replace_all(text, "\\$\\\\mathrm\\{([^}]+)\\}\\^\\{\\\\circ\\}\\s*([0-9]+)\\^\\{([0-9]+)\\}\\$", "\\1°\\2\\3")
  
  # 3.5 $\mathrm{n}^{\circ} 6$ -> n°6 (without exponent)
  text <- str_replace_all(text, "\\$\\\\mathrm\\{([^}]+)\\}\\^\\{\\\\circ\\}\\s*([0-9]+)\\$", "\\1°\\2")
  
  # 3.6 $\mathrm{C}^{\circ} .3$ -> C°.3 (dot and number)
  text <- str_replace_all(text, "\\$\\\\mathrm\\{([^}]+)\\}\\^\\{\\\\circ\\}\\s*([.][0-9]+)\\$", "\\1°\\2")
  
  # 3.7 $n^{\circ}$ -> n°
  text <- str_replace_all(text, "\\$([a-zA-Z]+)\\^\\{\\\\circ\\}\\$", "\\1°")
  text <- str_replace_all(text, "\\$\\\\mathrm\\{([^}]+)\\}\\^\\{\\\\circ\\}\\$", "\\1°")
  
  # 3.8 $4^{8}$ -> 48
  text <- str_replace_all(text, "\\$([0-9]+)\\^\\{([0-9]+)\\}\\$", "\\1\\2")
  
  # 3.9 $11^{\text {e }}$ -> 11e
  text <- str_replace_all(text, "\\$([0-9]+)\\^\\{\\\\text\\s*\\{\\s*([^{}]*)\\s*\\}\\}\\$", "\\1\\2")

  # 3.10 $\mathbf{M}^{\text {sos }}$ -> M sos
  text <- str_replace_all(text, "\\$\\\\mathbf\\{([^}]+)\\}\\^\\{\\\\text\\s*\\{\\s*([^}]+)\\s*\\}\\}\\$", "\\1 \\2")

  # After handling full expressions, proceed to general cleanup
  
  # 4. Extract content from remaining LaTeX tags (nested loop)
  for (i in 1:3) {
    text <- str_replace_all(text, "\\\\(text|mathrm|mathbf|textit|textbf|emph)\\s*\\{([^{}]*)\\}", "\\2")
  }

  # 4.1 Overline formulas $\overline{\mathrm{A}}$ -> A
  text <- str_replace_all(text, "\\$\\\\overline\\{\\\\mathrm\\{(.*?)\\}\\}\\$", "\\1")
  
  # 4.2 Remove common LaTeX commands
  text <- str_replace_all(text, "\\\\(left|right|underline|frac|sum|prod|int|lim|infty|begin|end|times|cdot|ldots|dots|partial|nabla|mathrm|overline)", "")
  
  # 4.3 Clean complete array structures and content (non-greedy)
  text <- str_replace_all(text, "\\\\begin\\{array\\}.*?\\\\end\\{array\\}", "")
  
  # 4.4 Clean common LaTeX environments and content (non-greedy)
  text <- str_replace_all(text, "\\\\begin\\{[^}]+\\}.*?\\\\end\\{[^}]+\\}", "")
  
  # 4.5 Remove escaped mathematical brackets
  text <- str_replace_all(text, "\\\\[\\[\\](){}]", "")
  
  # 4.6 Clean nested curly braces
  for (i in 1:3) { 
    text <- str_replace_all(text, "\\{[^{}]*\\}", "") 
  }
  
  # 5. Remove empty and isolated braces
  text <- str_replace_all(text, "\\\\\\{\\\\\\}", "")
  text <- str_replace_all(text, "\\{\\s*\\}", "")
  text <- str_replace_all(text, "\\\\\\{|\\\\\\}", "")
  
  # 6. Remove delimiters $...$ from remaining math formulas
  text <- str_replace_all(text, "(?s)\\$([^\\$]*?)\\$", "\\1")
  
  # 7. Final check to remove any remaining LaTeX commands
  text <- str_replace_all(text, "\\\\[a-zA-Z]+", "")
  
  # 8. Remove isolated backslashes or backslashes before non-letters
  text <- str_replace_all(text, "\\\\([^a-zA-Z0-9])", "\\1")
  
  # 9. Quotation marks normalization
  text <- str_replace_all(text, "\\\\\"", "\"")
  text <- str_replace_all(text, "''|``", "\"")
  text <- str_replace_all(text, "«\\s*", "\"")
  text <- str_replace_all(text, "\\s*»", "\"")
  
  # 10. Remove periods after closing quotes
  text <- str_replace_all(text, "\"\\. ", "\" ")
  
  # 11. Punctuation spacing
  text <- str_replace_all(text, "\\s*([;:!?])\\s*", "\\1 ")
  
  # 12. Exponents (outside math mode cleanup)
  text <- str_replace_all(text, "([0-9a-zA-Z])\\^\\{\\\\text ?\\{\\s*(.*?)\\s*\\}\\}", "\\1 \\2")
  text <- str_replace_all(text, "([0-9A-Za-z])\\^([a-zA-Z]{1,3})", "\\1\\2")
  text <- str_replace_all(text, "([0-9])\\^(?!(\\{|[a-zA-Z0-9]))", "\\1")
  
  # 13. Remove footnotes like [1]
  text <- str_replace_all(text, "\\[\\d+\\]", "")
  
  # 14. French quotes -> english quotes
  #text <- str_replace_all(text, "[«»]", '"')
  
  # 15. Common OCR Error Corrections
  
  # 15.1 Replace isolated "cl" with "d" (common OCR confusion)
  text <- str_replace_all(text, "\\bcl([,. ])", "d\\1")
  
  # 15.2 Fix spaces in scanned numbers
  text <- str_replace_all(text, "(\\d)\\s+(\\d{3})", "\\1\\2")
  text <- str_replace_all(text, "(\\d)\\s*,\\s*(\\d)", "\\1,\\2")
  
  # 15.3 Fix 'O' instead of '0' in numbers
  text <- str_replace_all(text, "\\bO([0-9])", "0\\1")
  text <- str_replace_all(text, "([0-9])O", "\\10")
  text <- str_replace_all(text, "\\bO,\\s*(\\d+)", "0,\\1")
  
  # 15.4 Fix 'I' instead of 'l' (lowercase L)
  text <- str_replace_all(text, "\\bI\\b(?![0-9\\s.,])", "l") #Remplace les i majuscule isolé en l
  text <- str_replace_all(text, "\\bI'([a-zéèêëàâäôöùûüïîç])", "l'\\1")
  text <- str_replace_all(text, "\\bi'([a-zéèêëàâäôöùûüïîç])", "l'\\1")
  text <- str_replace_all(text, "\\bI\\'", "l'")
  
  # 15.5 Fix accented characters encoded with backslashes
  text <- str_replace_all(text, "e\\\\´", "é")
  text <- str_replace_all(text, "e\\\\`", "è")
  text <- str_replace_all(text, "a\\\\`", "à")
  
  # 16. Punctuation and Space Corrections
  
  # 16.1 Clean multiple spaces and punctuation
  text <- str_replace_all(text, "\\s+([,.])\\s*", "\\1 ")
  text <- str_replace_all(text, "\\s+([;:!?])\\s*", "\\1 ")
  text <- str_replace_all(text, "([,])([,])+", "\\1")
  text <- str_replace_all(text, "\\.{3,}", "...")
  text <- str_replace_all(text, "[ \t]{2,}", " ")
  
  # 16.2 Parenthesis spacing
  text <- str_replace_all(text, "\\(\\s+", "(")
  text <- str_replace_all(text, "\\s+\\)", ")")
  
  # 16.3 Fix excessive spacing in dates (French months)
  text <- str_replace_all(text, "\\b([0-9]{1,2})\\s+([jJ]anvier|[fF]évrier|[mM]ars|[aA]vril|[mM]ai|[jJ]uin|[jJ]uillet|[aA]oût|[sS]eptembre|[oO]ctobre|[nN]ovembre|[dD]écembre)\\s+([0-9]{4})\\b", "\\1 \\2 \\3")
  
  # 16.4 Final punctuation spacing cleanup
  text <- str_replace_all(text, "\\s+([,.!?;:])", "\\1")
  text <- str_replace_all(text, "([,.!?;:])(?=[^\\s\\d\\)])", "\\1 ")
  
  # 16.5 Trim leading and trailing spaces
  text <- str_replace_all(text, "^[ \t]+|[ \t]+$", "")

  # 17 Erase title marker
  text <- str_replace_all(text, "#", "")

  # 18 Erase *
  text <- str_replace_all(text, "\\*", "")
  
  return(text)
}