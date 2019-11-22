# =======================
# XML Grammar Parser
# =======================
# Erich Round
# Created 2017
# Last updated 2018-03-02
# =======================

library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
source("xml2_extras.R")  # extra functions for manipulating XML

preprocessed_directory  <- "grammars_txt_and_html"
processed_html_directory  <- "grammars_txt_and_html"
processed_xml_directory <- "grammars_processed_xml"
temp_directory <- "temp_files"
reports_directory <- "grammars_reports"
load("English_word_list.RData")  # loads the string vector English_word_list


###################
###################
### Batch functions
###################
###################

# We are working through a large number of pdf grammars.
# Here: look for newly OCR-processed grammars and apply
# the grammar harvester to them.
harvest_new_abbyy_html = function() {
	get_new_filename_bases() %>%
		harvest_multiple_Abbyy_html()
}

# Apply the grammar harvester to ALL OCR-processed grammars.
harvest_all_Abbyy_html = function() {
	get_all_filename_bases() %>%
		harvest_multiple_Abbyy_html()
}

# TRY to apply the grammar harvester to those files whose
# filename_bases are provided.
# Here, if the harvester fails on any one of the OCR-
# processed files, it just moves to the next one.
# By default, it will look just for new grammars
harvest_multiple_Abbyy_html = function(filename_bases = get_new_filename_bases()) {
	print(filename_bases)
	for (f in filename_bases) {
		cat("====", f, "====\n")
		try(harvest_Abbyy_html(f))
	}
}


##############################
##############################
## The main Harvester function
##############################
##############################

# This top-level function reads in the input files
# and calls a series of high-level functions, to build
# new XML and HTML structures, and then saves them.
#
# In doing so, it also calls upon grammar-specific parameter
# files. These are manually prepared, and assist the process
# by flagging various observed properties of the grammar.
# General parameters are stored in the file:
# - base_filename + _params.txt
# A table of abbreviations is in the file:
# - base_filename, but with _abbrs_ in place of _body_

harvest_Abbyy_html = function(filename_base, skip_to_analyse_ex = FALSE) {
	# filename_base  is a filename without any extension
	# It'll include the text "_body_", which'll get replaced with
	# "_augmented_" in the output files.
	#
	# skip_to_analyse_ex  is a flag, if TRUE, skip the early part 
	# of building the XML file.

	# Load the html file, whose filename may end .html or .htm
	infile_html <- file.path(
		preprocessed_directory, 
		str_c(filename_base, ".html"))
	if (!file.exists(infile_html)) {
		infile_html <- str_replace(infile_html, "html$", "htm")
	}
	
	# Set the file paths for output files, which will eventually contain:
	# - outfile_augmented_xml: the XML-marked-up grammar
	# - outfile_augmented_html: a human readable version of the marked-up grammar
	# - outfile_layout_xml: an intermediate structure, used for bug checking
	# - outfile_layout_html: an intermediate structure, used for bug checking
	outfile_augmented_xml <- file.path(
		processed_xml_directory, 
		str_c(filename_base, ".xml") %>% str_replace("_body_", "_augmented_"))
	outfile_augmented_html <- file.path(
		processed_html_directory, 
		str_c(filename_base, ".html") %>% str_replace("_body_", "_augmented_"))
	outfile_layout_xml <- file.path(
		processed_xml_directory, 
		str_c(filename_base, ".xml") %>% str_replace("_body_", "_layout_"))
	outfile_layout_html <- file.path(
		processed_html_directory, 
		str_c(filename_base, ".html") %>% str_replace("_body_", "_layout_"))


	# Create a "layout" representation, which explicitly records aspects
	# of the document _layout_.
	# Then create an "augemented" representation in which aspects of layout
	# and literal content are marked-up for their function

	if(!skip_to_analyse_ex) {

		# skip_to_analyse_ex == FALSE
		# build layout_xml and augmented_xml from scratch

		layout_xml <- 
			# Convert the input file from HTML to XML format
			html_2_xml(read_html(infile_html)) %>%
			# DO SOMETHING
			create_layout_level_from_Abbyy_html(filename_base = filename_base) %>%
			# Clean up OCR junk (currently only used for one grammar)
			clean_OCR_nonalpha(filename_base = filename_base)
		
		## For bug checking:
		# write_xml(layout_xml, file = outfile_layout_xml) 
		# transform_layout_xml_to_html(
		# 	xml_file = outfile_layout_xml,
		# 	html_outfile = outfile_layout_html)

		augmented_xml <-
			layout_xml %>%
			# In the document, identify and mark up instances of
			# the use of the abbreviations which were defined by the
			# grammar's author. A table of these abbreviations is 
			# prepared manually during the OCR process, and is used now.
			annotate_abbreviations(filename_base = filename_base) %>%
			# Linguists often italicise vernacular words which are
			# cited in the main text. Find these and mark them up.
			annotate_italicized_vernacular_words() %>%
			# Sometimes, a linguistic example occurs in the body text.
			# Attempt to identify these and mark them up
			annotate_intext_examples() %>%
			# I have two versions of an algorithm for locating, analysing
			# and marking up interlinear glossed text (IGT). The first one 
			# used a set of hard-coded heuristics to infer the location of 
			# IGT and its parts; for the second one  Itook the results of 
			# the first, after it'd been applied to >100 grammars, and built
			# a simle statistical model.
			locate_interlinear_text(filename_base = filename_base) %>%
			# IGT consists of multiple lines of text, each with one of
			# a small set of functions. These have an implicit heirarchical
			# structure with repect to one another. Attempt to infer that
			# structure (the process works okay, but is not failsafe.)
			group_interlinear_text(filename_base = filename_base)
	
	} else {

		# skip_to_analyse_ex == TRUE
		# load augmented XML from previously saved file
		augmented_xml <- 
			read_xml(file.path(temp_directory, "grouping_complete.xml"))
	}

	# Having identified the IGT, and the classes of functions of
	# the individual lines, now analyse it further; then represent
	# paragraphs in the body text; and assign IDs
	augmented_xml <-
		augmented_xml %>%
	 	# Analyse IGT into words and morphemes.
		analyse_interlinear_text(filename_base = filename_base) %>%
		# Analyse morphemes, ie., morph::gloss mappings
		analyse_morphemes(filename_base = filename_base) %>%
		# Build representations of paragraphs instead of just lines
		merge_paras_across_pages() %>%
		dissolve_lines_add_sentence_nodes() %>%
		# Assign IDs to all units
		assign_id_to_all()

	# Save the XML representation
	write_xml(augmented_xml, file = outfile_augmented_xml)

	# Create a nice human-readable version of the analysed
	# grammar
	transform_augmented_xml_to_html(
		xml_file = outfile_augmented_xml,
		html_outfile = outfile_augmented_html)

	# Compile a report on the content of IGT.
	# Reported examples are ordered by example number
	# (head number and subnumber) and any apparent gaps
	# are flagged with "**".
	# Examples which earlier were tagged as problematic
	# are flagged in the report with "!"
	# In an earlier version, I also had code that found
	# all morphemes, the apparent allomorphs of each
	# gloss, and apparent alloglosses of each morph. I
	# haven't implemented that here yet.
	cat("Compiling examples report\n")
	report_examples(filename_base)
}


#######################
#######################
## High-level functions
#######################
#######################

create_layout_level_from_Acrobat_html = function(infile) {
	# TO DO
	# TO DO
}

# Add a copy of hte <body> node, called <layout_xml>
# Strip away text styles except for italics
# Create <line> nodes and <page_break/> nodes
# Simplify text & styles in tables
create_layout_level_from_Abbyy_html = function(infile, filename_base) {
	infile %>%
		# xml_remove_node_type("a") %>%         # remove hyperlinks
		add_basic_layout_xml_node() %>%
		simplify_Abbyy_text_styles() %>%
		handle_pages_paras_and_lines(filename_base = filename_base) %>%
		simplify_Abbyy_table_text() 
}

# Not sure what this function is/was for. Not currently used.
create_aug_layout_level = function(filename_base) {
	xml_file <- file.path(processed_xml_directory, str_c(filename_base, ".xml"))
	abbrs <- load_txt_abbreviations(filename_base)

	read_xml(xml_file) %>%
		add_aug_layout_node()
}

## Mid-level functions

# Add a "layout" XML node
#
# Adds a top node <grammar_xml> and retains the
#   input html node below that as <Abbyy_html>, and
#   a clone of it (for later processing) as <layout_mxl>.
# Changes the files headers.
# Converts &lt; etc. into single characters.
# Add IDs to paragraphs, headers and table rows.
# Checks whether there actually are any paragraph
#   nodes, and halts if not.
add_basic_layout_xml_node = function(html_doc) {
	cat(str_c(Sys.time()), "HTML_to_XML_conversion\n")	
	# cat(str_c(Sys.time()), "add_basic_layout_xml_node\n")
	# Add top node, "grammar_xml", above <html>
	current_root <- xml_find_all(html_doc, ".")
	xml_name(current_root) <- "Abbyy_html"

	xml_doc <- xml_insert_parent_layer(
		xml_doc = html_doc, 
		target_node = current_root, 
		parent_name = "grammar_xml")

	# Change the headers of the file
	# Change <, > and & into the unicode "small" variants thereof
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("<!DOCTYPE[^>]*>", "&lt;", "&gt;", "&amp;", "&#13;"),
		replacement_vec = c("", "\uFE64", "\uFE65", "\uFE60", "\n"))

	# Add id's to <p>, <h1>, <tr> nodes
	xml_find_all(xml_doc, "//p | //tr | //h1") %>% 
		xml_assign_consecutive_ids(attr_name = "html_id", prefix = "seq_")

	# write_xml(xml_doc, file.path(temp_directory, "pre_clone.xml"))

	# Make clone of <Abbyy_html>'s <body> node, as <layout_xml>
	xml_txt <- xml_doc_to_xml_string(xml_doc)
	pre_body_regex <- "( *<[^/].*\n)+( *</head>\n)"
	post_body_regex <- "</Abbyy_html>(\n|.)*"
	xml_doc <- 
		xml_txt %>%
		str_replace(pre_body_regex, "") %>%
		str_replace(post_body_regex, "") %>%
		str_replace("<body[^>]*>", "<layout_xml>") %>%
		str_replace("</body>", "</layout_xml>") %>%
		str_c(
			str_replace(xml_txt, "</grammar_xml>", ""), 
			., 
			"\n</grammar_xml>") %>%
		read_xml()

	# Here's the old code, but it fails on very large documents:
	
	# xml_doc <- xml_str_replace_all(xml_doc,
	# 	pattern_vec = "(<body[^>]*>)((.*\n)+ *)</body>(\n *)?</Abbyy_html>",
	# 	replacement_vec = "\\1\\2</body>\\4</Abbyy_html><layout_xml>\\2</layout_xml>")

	# Check that there are paragraph nodes
	all_paras <- xml_find_all(xml_doc, "//layout_xml//p")
	n_paras <- length(all_paras)
	if (n_paras == 0) { 
		write_xml(xml_doc, file.path(temp_directory, "no_p_nodes.xml"))
		stop("No paragraph nodes found (of type //layout_xml//p).")
	}

	return(xml_doc)
}

# Simplify text styles in OCR output
#
# Removes small caps and bold; preserves
# italics and underline but as <i>, <u> and <ui>
# nodes rather than as <span> nodes. Removes
# all <span> nodes.
simplify_Abbyy_text_styles = function(xml_doc) {
	# cat(str_c(Sys.time()), "simplify_Abbyy_text_styles\n")	

	# Note all @style attributes of <span> nodes
	styled_span_nodes <- xml_find_all(xml_doc, "//layout_xml//span[@style]")
	style_attrs <- xml_attr(styled_span_nodes, "style")
	sc_node_indices <- which(str_detect(style_attrs, "small-caps"))
	i_node_indices <- which(str_detect(style_attrs, "italic"))
	u_node_indices <- which(str_detect(style_attrs, "underline"))

	# Remove all @class and @style attributes
	class_attrs <- xml_find_all(xml_doc, "//layout_xml//span/attribute::class")
	style_attrs <- xml_find_all(xml_doc, "//layout_xml//span/attribute::style")
	if (length(class_attrs) > 0) { xml_remove(class_attrs) }
	if (length(style_attrs) > 0) { xml_remove(style_attrs) }

	# Convert small caps to caps
	target_nodes <- xml_find_all(styled_span_nodes[sc_node_indices], "descendant::text()")
	if (length(target_nodes) > 0) {
		xml_text(target_nodes) <- str_to_upper(xml_text(target_nodes))
	}

	# Convert styled <span>s into <i>, <u>, <ui>
	i_only_indices <- setdiff(i_node_indices, u_node_indices)
	u_only_indices <- setdiff(u_node_indices, i_node_indices)
	ui_indices <- intersect(i_node_indices, u_node_indices)
	if (length(i_only_indices) > 0) {
		i_nodes <- styled_span_nodes[i_only_indices]
		xml_name(i_nodes) <- "i"	
	}
	if (length(u_only_indices) > 0) {
		u_nodes <- styled_span_nodes[u_only_indices]
		xml_name(u_nodes) <- "u"	
	}	
	if (length(ui_indices) > 0) {
		ui_nodes <- styled_span_nodes[ui_indices]
		xml_name(ui_nodes) <- "ui"	
	}

	# Collapse remaining span and font nodes into their parents
	xml_doc <- 
		xml_collapse_nodes_upwards(xml_doc, 
			nodes = xml_find_all(xml_doc, "//layout_xml//span"))

	font_nodes <- xml_find_all(xml_doc, "//layout_xml//font")
	if (length(font_nodes) > 0) {
			xml_doc <- xml_collapse_nodes_upwards(xml_doc, font_nodes)
	}

	return(xml_doc)
}

# Make representations of paragraphs, lines and pages
#
# From html <p> nodes whose internal lines are delimited
#   by <br/>s, creates <para> nodes with internal 
#   <line> nodes instead.
# Add <page_break/> nodes. This is done based on text that
#   was added, in Adobe Acrobat, to the top of every page
#   in the grammars, prior to OCRing.
# Mark sections of text that are bounded by tables:
#   this is necessary so that later functions don't
#   try to infer IGT examples that would span across
#   an intervening table.
# Remove empty lines.
handle_pages_paras_and_lines = function(xml_doc, filename_base) {
	# cat(str_c(Sys.time()), "handle_pages_paras_and_lines\n")		

	# Remove <div> nodes, and <br clear="all"/>
	target_nodes1 <- xml_find_all(xml_doc, "//*[@clear]")
	target_nodes2 <- xml_find_all(xml_doc, "//layout_xml//div")
	xml_remove(target_nodes1)
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, target_nodes2)

	# Change <h1>. <h2>, etc. to <para> but note Abbyy head header
	target_nodes <- xml_find_all(xml_doc, 
		str_c("//layout_xml/descendant::h", 1:9) %>% 
		str_c(collapse = " | "))
	if (length(target_nodes) > 0) {
		xml_attr(target_nodes, "Abbyy_header") <- "TRUE"
		xml_name(target_nodes) <- "para"
	}

	# Remove <a name=X> nodes
	target_nodes <- xml_find_all(xml_doc, "//layout_xml/descendant::a[@name]")
	if (length(target_nodes) > 0) {
		xml_remove(target_nodes)
	}

	# Change <p> to <para> and <br> to <break>
	para_nodes <- xml_find_all(xml_doc, "//layout_xml//p")
	xml_name(para_nodes) <- "para"
	break_nodes <- xml_find_all(xml_doc, "//layout_xml//br")
	if (length(break_nodes) > 0) { xml_name(break_nodes) <- "break" }

	# Put a single <line> node as the immediate
	# child of all <para>
	xml_doc <- xml_insert_child_layer(
		xml_doc = xml_doc,
		target_nodes = xml_find_all(xml_doc, "//para"),
		child_name = "line")

	# Change page breaks to a <page_break/> node, and
	# tuck it inside any following <para> node.
	break_regex <- str_c(
		"<para[^/>]*>(?:\\n *)?<line>",
		"BREAK\\s?HERE\\s?(?:PAGE\\s?)?",
		"([0-9]+)",  # this is capturing group \\1
		"</line>(?:\\n *)?</para>")
	xml_doc <- 
		xml_str_replace_all(xml_doc,  
			pattern_vec = c(
				"PAG El ",
				str_c(break_regex, "(?:\\n *)?",
					"(<para[^/>]*>)"),  # this is capturing group \\2
				break_regex),
			replacement_vec = c(
				"PAGE1",  # fix a not-uncommon OCR mistake
				"\\2<page_start page_number='\\1'/>",  # for page breaks followed by <para>
				"<page_start page_number='\\1'/>"   # for page breaks followed by non-<para> nodes
				))

	write_xml(xml_doc, file.path(temp_directory, "temp00.xml"))

	# If a page break at the start of a <para> node 
	# is followed by text that could be an example number, 
	# or by a numeral (which is likely a part of a section 
	# heading), tuck it into the end of the previous <para>.
	num_or_ex_num_regex <- 
		get_parameter("ex_number_regex", filename_base) %>%
		str_replace("^\\^", "") %>%
		str_c("([0-9]|", ., ")")
	target_regex <- str_c(
		"(</para>(?:\\n *)?",
		"<para[^/>]*>(?:\\n *)?)",  # capturing group 1
		"(<page_start[^>]*/>(?:\\n *)?)",     # capturing group 2
		"(<line>", num_or_ex_num_regex, ")"   # capturing group 3
		)
	xml_doc <- 
		xml_str_replace_all(xml_doc,
			pattern_vec = target_regex,
			replacement_vec = "\\2\\1\\3")


	# Put <break/>s outside, not inside, the edges
	# of <i>, <u>, <ui>, <sup>, <sub> nodes
	xml_doc <- xml_doc %>%
		xml_repel(repeller = "i", repelled = "break") %>%
		xml_repel(repeller = "u", repelled = "break") %>%
		xml_repel(repeller = "ui", repelled = "break") %>%
		xml_repel(repeller = "sup", repelled = "break") %>%
		xml_repel(repeller = "sub", repelled = "break")

	# Split <line> nodes where currently there is
	# a <br/> daughter or granddaughter of a <line>
	target_brs <- xml_find_all(xml_doc, "//line/break")
	if (length(target_brs) > 0) {
		xml_name(target_brs) <- "_replace_me_"
		xml_doc <- 
			xml_str_replace_all(xml_doc,
				pattern_vec = "<_replace_me_/>",
				replacement_vec = "</line><line>")
	}
	target_brs <- xml_find_all(xml_doc, "//line/i/break")
	if (length(target_brs) > 0) {
		xml_name(target_brs) <- "_replace_me_"
		xml_doc <- 
			xml_str_replace_all(xml_doc,
				pattern_vec = "<_replace_me_/>",
				replacement_vec = "</i></line><line><i>")
	}
	target_brs <- xml_find_all(xml_doc, "//line/u/break")
	if (length(target_brs) > 0) {
		xml_name(target_brs) <- "_replace_me_"
		xml_doc <- 
			xml_str_replace_all(xml_doc,
				pattern_vec = "<_replace_me_/>",
				replacement_vec = "</u></line><line><u>")
	}
	target_brs <- xml_find_all(xml_doc, "//line/ui/break")
	if (length(target_brs) > 0) {
		xml_name(target_brs) <- "_replace_me_"
		xml_doc <- 
			xml_str_replace_all(xml_doc,
				pattern_vec = "<_replace_me_/>",
				replacement_vec = "</ui></line><line><ui>")
	}
	target_brs <- xml_find_all(xml_doc, "//line/sup/break")
	if (length(target_brs) > 0) {
		xml_name(target_brs) <- "_replace_me_"
		xml_doc <- 
			xml_str_replace_all(xml_doc,
				pattern_vec = "<_replace_me_/>",
				replacement_vec = "</sup></line><line><sup>")
	}
	target_brs <- xml_find_all(xml_doc, "//line/sub/break")
	if (length(target_brs) > 0) {
		xml_name(target_brs) <- "_replace_me_"
		xml_doc <- 
			xml_str_replace_all(xml_doc,
				pattern_vec = "<_replace_me_/>",
				replacement_vec = "</sub></line><line><sub>")
	}

	write_xml(xml_doc, file.path(temp_directory, "temp01.xml"))

	# Mark first and last lines in sections of text bounded by
	# tables or images
	last_lines <- xml_find_all(xml_doc,
		str_c("//table/preceding::*[1]/ancestor-or-self::line | ",
			"//img/preceding::*[1]/ancestor-or-self::line"))
	first_lines <- xml_find_all(xml_doc,
		str_c("//table/following::*[1]/ancestor-or-self::line | ",
			"//img/following::*[1]/ancestor-or-self::line"))
	xml_attr(first_lines, "start_body_text") <- "TRUE"
	xml_attr(last_lines, "end_body_text") <- "TRUE"

	# Delete lines that are empty
	all_lines <- xml_find_all(xml_doc, "//para/line")
	empty_lines <- which(
		str_length(str_trim(xml_text(all_lines))) == 0)
	xml_name(all_lines[empty_lines]) <- "empty_line"
	xml_doc <- xml_remove_node_type(xml_doc, "empty_line")


	all_lines <- xml_find_all(xml_doc, "//para/line")
	n_lines <- length(all_lines)
	if (n_lines == 0) stop("No nodes found of type //para/line.")

	return(xml_doc)
}

# This function is actually specific to just one grammar:
# Menggwa-Dla_112103_body_desousa_2006
clean_OCR_nonalpha = function(xml_doc, filename_base) {
	if (!is_parameter_true("clean_OCR_nonalpha", filename_base)) { return(xml_doc) }

	trigger_regex <- "[148\\$]"
	text_nodes <- xml_find_all(xml_doc, "//layout_xml/descendant::text()")
	node_text <- xml_text(text_nodes)
	candidate_nodes <- which(str_detect(node_text, trigger_regex))
	text_nodes <- text_nodes[candidate_nodes]
	node_text <- node_text[candidate_nodes]

	English <- get_English_word_list()
	candidate_words <- str_c(node_text, collapse = " ") %>%
		str_split("[,.? \u00A0]+") %>%
		unlist() %>% unique() %>%
		subset(str_detect(., trigger_regex) & str_detect(., "[a-z]"))
	alterations_df <- data.frame(
			original = candidate_words,
			altered = candidate_words %>%
				str_replace_all("1", "l") %>%
				str_replace_all("4", "i") %>%
				str_replace_all("8", "v") %>%
				str_replace_all("\\$", "s"),
			stringsAsFactors = FALSE
			) %>% 
		rowwise() %>%
		mutate(is_fixed = str_to_lower(altered) %in% English) %>%
		filter(is_fixed)
	n_alterations <- nrow(alterations_df)
	names_node <- which(str_detect(node_text, "name\\$"))

	if (n_alterations > 0) {
		cat(str_c(Sys.time()), "Cleaning OCR in", n_alterations, "word types\n")
		for (a in 1:nrow(alterations_df)) {
			node_text <- str_replace_all(
				node_text, 
				fixed(alterations_df$original[a]),
				fixed(alterations_df$altered[a]))
		}
	xml_text(text_nodes) <- node_text
	}
	return(xml_doc)
}

# Simplify html table text
#
# Removes: 
# - multiple <para> nodes within the same table cell
# - table border & style attributes
simplify_Abbyy_table_text = function(xml_doc) {
	# cat(str_c(Sys.time()), "simplify_Abbyy_table_text\n")		
	# Collapse <p> nodes in table cells
	para_in_td_nodes <- xml_find_all(xml_doc, "//layout_xml//td/para")
	if (length(para_in_td_nodes) > 0) {
		xml_doc <- 
			xml_collapse_nodes_upwards(xml_doc, nodes = para_in_td_nodes)
	}

	# Remove table border & style attributes
	border_attrs <- xml_find_all(xml_doc, "//layout_xml//table/attribute::border")
	style_attrs  <- xml_find_all(xml_doc, "//layout_xml//td/attribute::style")
	if (length(border_attrs) > 0) { xml_remove(border_attrs) }
	if (length(style_attrs) > 0)  { xml_remove(style_attrs) }

	return(xml_doc)
}

# Annotate the author's abbreviations
#
# Reads in the _abbr_ file and then identifies those
# abbrevations in the text. Attempts to do so in a manner
# that's somewhat robust to OCR errors.
# Once found, annotates them with an <abbr> node.
annotate_abbreviations = function(xml_doc, filename_base) {
	if (is.null(xml_doc)) stop("xml_doc is NULL")
	cat(str_c(Sys.time()), "preliminary_annotations\n")	
	# cat(str_c(Sys.time()), "annotate_abbreviations\n")	

	abbr_file <- file.path(
			preprocessed_directory, 
			str_c(filename_base, ".txt") %>% str_replace("_body_", "_abbr_"))

	# If no _abbrs_ file is supplied then exit.
	if (!file.exists(abbr_file)) { 
		# warning(str_c("No abbreviation file for ", 
		# 	filename_base, "."))
		return(xml_doc)
	}

	# Load txt abbreviations
	# Arrange them by decreasing length to ensure longest 
	# possible is detected first
	abbrs <- read.table(file = abbr_file,
		sep = "\t", quote = "", comment = "", header = FALSE)
	colnames(abbrs) <- c("abbr", "full")
	abbrs <- abbrs %>%
		filter(!str_detect(abbr, "^[0-9Aa]$")) %>%   # Remove abbrs that will produce too many false hits
		arrange(-nchar(as.character(abbr))) 

	unescaped_metacharacters <- "(?<!\\\\)(\\(|\\)|\\[|\\]|\\{|\\}|\\.|\\$|\\^|\\+|\\*|\\?|\\\\|\\|)"
	abbr_vec <- 
		abbrs$abbr %>%
		setdiff(c(str_c(0:9), "")) %>%
		str_replace_all(unescaped_metacharacters, "\\\\\\1") %>%
		str_replace_all("<", "\uFE64") %>%  # Change <, > and & into the unicode "small" variants thereof
		str_replace_all(">", "\uFE65") %>%
		str_replace_all("&", "\uFE60")

	# Make abbreviations more robust to OCR noise
	orig <- letters[1:26]
	robust <- c("[Aa]", "[Bb]", "[Cc]", "[Dd]", "[Ee]", "[Ff]",
		"[GCg]", "[Hh]", "[Iil1]", "[Jj]", "[Kk]", "[Lli1]",
		"[Mm]", "[Nn]", "[Oo0]", "[Pp]", "[Qq]", "[Rr]", "[Ss5]",
		"[Tt]", "[Uu]", "[Vv]", "[Ww]", "[Xx]", "[Yy]", "[Zz]")
	abbr_vec <- str_to_lower(abbr_vec)
	for (i in 1:26) {
		abbr_vec <- str_replace(abbr_vec, orig[i], robust[i])
	}
	abbr_regex <- str_c("(", str_c(abbr_vec, collapse = "|"), ")")

	# Find abbreviations bounded either by spaces or by
	# morph boundary symbols
	morph_edge_left_regex <-"([A-Za-z0-9][-=+:\uFE64\uFE65.])" 
	morph_edge_right_regex <-"([-=+:\uFE64\uFE65.][A-Za-z0-9])"
	pattern1 <- str_c(
		morph_edge_left_regex, abbr_regex, 
		"(", morph_edge_right_regex, "|[ \u00A0])")
	pattern2 <- str_c(
		"(", morph_edge_left_regex, "|[ \u00A0])",
		abbr_regex, morph_edge_right_regex)
	text_nodes <- xml_find_all(xml_doc, "//self::text()")
	node_text <- xml_text(text_nodes)
	xml_text(text_nodes) <- 
		node_text %>%
		str_replace_all(pattern1, "\\1♕\\2♔\\3") %>%   # ♕ and ♔ are place-holders for the replacement command below
		str_replace_all(pattern2, "\\1♕\\3♔\\4")
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("♕", "♔"),
		replacement_vec = c("<abbr>", "</abbr>"))
}

# Annotate italicized vernacular words
#
# Tags all italicized words that aren't in the vector
# English_word_list, as <v>. To see how English_word_list was
# assembled, look at comments in get_English_word_list().
# Italicized English is retained as <i>.
annotate_italicized_vernacular_words = function(xml_doc) {
	if (is.null(xml_doc)) stop("xml_doc is NULL")

	italics_text_nodes <- xml_find_all(xml_doc, "//i/descendant::text()")
	if (length(italics_text_nodes) == 0) { return (xml_doc) }

	# cat(str_c(Sys.time()), "annotate_italicized_vernacular_words\n")	
	italics_words <- 
		xml_text(italics_text_nodes) %>%
		str_split("[ \u0A00]+") %>%
		unlist() %>% unique()

	is_nonVernac_italics <- count_English(italics_words)   # a dataframe with columns word_count, English_count
	is_nonVernac_italics$word <- italics_words
	is_nonVernac_italics <- 
		mutate(is_nonVernac_italics,
			is_nonVenac = 
				(English_count == 1) | 
				!str_detect(word, "[a-z]")    # make sure Vernac has at least one letter
			) %>%
		.$is_nonVenac
	nonVernac <- italics_words[is_nonVernac_italics]  # a vector of words identified as non-vernacular italicised

	# Process each <i> node in turn
	for (n in italics_text_nodes) {
		node_text <- xml_text(n) %>% str_replace_all(" ", "\u00A0")   # Use non-breaking space to prevent spaces ending up being treated as XML code spacing, when they should be text
		words <- str_split(node_text, "\u00A0+")[[1]]  # a vector of all the words
		all_indices <- 1:length(words)
		spaces <- str_extract_all(node_text, "\u00A0+")[[1]]  # a vector of all the spaces
		
		# Note which italicized words are merely italicized, and
		# which are vernacular
		if (str_detect(node_text, "^[0-9]+\\.[0-9]")) {
			# Italicized section titles
			to_italic <- all_indices
			to_vernac <- integer(0)
		} else {
			# Anything else italicized
			to_italic <- which(
				words %in% nonVernac |
				str_detect(words, "[0-9]"))
			to_vernac <- setdiff(all_indices, to_italic)
		}
		words[to_vernac] <- str_c("♔", words[to_vernac], "♕")  # add delimiters for the replace command below
		words[to_italic] <- str_c("♖", words[to_italic], "♗")  # add delimiters for the replace command below
		xml_text(n) <- str_c(words, c(spaces, ""), collapse = "")
	}

	# Remove the existing <i> nodes
	italics_nodes <- xml_find_all(xml_doc, "//i")
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, italics_nodes)

	# Put English words in <i>, and vernacular words in <v>
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("♔", "♕", "♖", "♗", 
			"([,:;?!.])(</v>)",  # move punctuation outside of the <v> node
			"(<v>)([\\(\\[\\{\"'‘“])",  # move bracketing symbols outside of the <v> node
			"([\\)\\]\\}\"'’”][,:;?!.]?)(</v>)"),    
		replacement_vec = c("<v>", "</v>", "<i>", "</i>",
			"\\2\\1", "\\2\\1", "\\2\\1"))
}

# Annotate in-text examples as <intx>
#
# Searches for a <v> node (in-text vernacular) followed,
# after 0 or more spaces, by text within quotes.
annotate_intext_examples = function(xml_doc) {
	if (is.null(xml_doc)) stop("xml_doc is NULL")
	# cat(str_c(Sys.time()), "annotate_intext_examples\n")
	v_node_regex <- "((<v>[^<]+</v>\\s*)*<v>[^<]+</v>)\\s*"
	quote_regex <- "([\"'‘“][^<\"'’”]+[\"'’”])"
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = str_c(v_node_regex, quote_regex),
		replacement_vec = "<intx>\\1<intxg>\\3</intxg></intx>")	
	return(xml_doc)
}

# Locate and tag IGT lines by their function
#
# This uses a model of IGT line-by-line syntax shown below
# Abbreviations an mnenomic:
# x   example
# h   head example number
# s   subhead example number
# 0   no example numer
# g   gloss or gloss group (vernacular + related gloss)
# f   free trans or free trans group (set of lines in a free translantion)
# fn  free trans, nonfinal line
# ff  free trans, final line
# u   unparsed vernacular
# v   parsed vernacular
#
# x -> xh xs*
# xh	->	xfh xf0*
# xs	->	xfs xf0*
# xfh	->	xgh xg0* xft
# xfs	->	xgs xg0* xft
# xf0	->	xg0 xg0* xft
# xft	->	xftn* xftf
#
# glossing — if you expect unparsed + parsed vernacular lines
# xgh	->	xuh (xv0 | xu0) xg 
# xgs	->	xus (xv0 | xu0) xg
# xg0	->	xu0 (xv0 | xu0) xg
#
# glossing — expecting only unparsed
# xgh	->	xuh xg
# xgs	->	xus xg
# xg0	->	xu0 xg
#
# glossing — expecting only parsed
# xgh	->	(xvh | xuh) xg
# xgs	->	(xvs | xus) xg
# xg0	->	(xv0 | xu0) xg
#
#
# The process is to use a large number of descriptive
# factors for each line, to infer which of the above IGT types
# the line is, and then attempt to parse the lines into
# example sentences with the right syntax
#
#
# When the inference is that done, all lines are tagged 
# according to their inferred function.
locate_interlinear_text = function(xml_doc, filename_base) {
	## Create df of lines' text and various properties of it
	## Use that to infer the function of the lines:
	## xv - morph-parsed verncular example text
	## xu - unparsed verncular example text
	## xg - gloss line
	## xf - free translation line
	## nonx - non-example line
	if (is_parameter_true("lacks_numbered_examples", filename_base)) { return(xml_doc) }

	cat(str_c(Sys.time()), "locate_interlinear_text\n")	

	# Load up the grammar's parameters
	ex_number_regex <- get_parameter("ex_number_regex", filename_base)
	if (!nzchar(ex_number_regex)) { ex_number_regex <- "♔" }  # This is to circumvent problem that pattern="" can result in false matches
	ex_head_number_regex <- get_parameter("ex_head_number_regex", filename_base)
	ex_sub_number_regex <- get_parameter("ex_sub_number_regex", filename_base)
	if (!nzchar(ex_sub_number_regex)) { ex_sub_number_regex <- "♔" }  # This is to circumvent problem that pattern="" can result in false matches
	quoted_xft <- is_parameter_true("expect_quoted_free_translation", filename_base)
	expect_xg_xg <- is_parameter_true("expect_adjacent_gloss_lines", filename_base)
	expect_xh <- is_parameter_true("expect_example_header_lines", filename_base)
	expect_xv <- is_parameter_true("expect_parsed_vernacular_lines", filename_base)
	expect_xu <- is_parameter_true("expect_unparsed_vernacular_lines", filename_base)
	if (!expect_xv & !expect_xu) { 
		warning(str_c(filename_base, 
			" - Faulty expectations in param file: ",
			"neither parse nor unparsed vernacular."))
	}
	xf_identifier <- get_parameter("language_identifier_xf_line_regex", filename_base)
	
	# Set up regex expressions
	juncture_regex <- "([A-Za-z0-9][-=+:\uFE64\uFE65][A-Za-z0-9])|([A-Za-z]\\.[A-Za-z])"
	juncture_distractors <- str_c(collapse = "|", 
		c("-level", "-final", "-initial", "-medial", "-like",
			"sub-", "inter-", "intra-", "cross-", "extra-", 
			"^-", "-$"))
	quote_regex <- "[\"'‘“].+[\"'’”]|^[^\"'‘“]+[\"'’”]$|^[\"'‘“][^\"'’”]+$"
	left_quote_regex <- "^[\"'‘“]"
	final_punct_regex <- "[.?!:\"'‘’“”]$"
	metadata_regex <- "\\s*(\\[|\\().+(\\]|\\))(?!.*[\"'’”])"  # Doesn't find bracketted stuff before a closing quote
	final_continuers <- "[A-Za-z,-;]\\s*$"  # likely signal that a line and its following line cohere
	initial_continuers <- "^\\s*[a-z]"  # likely signal that a line and its preceding line cohere
	final_terminaters <- "[!?.][\"'’”\\)\\]]?\\s*$"

	# Assign line IDs and read their contents
	all_lines <- xml_find_all(xml_doc, "//para/line")
	n_lines <- length(all_lines)
	if (n_lines == 0) stop("No nodes found of type //para/line.")
	xml_assign_consecutive_ids(all_lines, attr_name = "line_id")
	all_lines_text <- xml_text(all_lines)

	# Set up a data frame where all the factors for each
	# line will go
	line_df <- data.frame(stringsAsFactors = FALSE,
		line_id = 1:n_lines, 
		text = all_lines_text)

	# cat(str_c(Sys.time()), "count vernacular & abbreviations\n")	

	# Tag lines containing abbreviations	
	target_lines <- xml_find_all(xml_doc, "//abbr/ancestor::line")
	xml_attr(target_lines, "has_abbreviation") <- "TRUE"
	target_lines <- xml_find_all(xml_doc, 
		"//line[@has_abbreviation]/child::abbr[position() > 1]/ancestor::line")
	xml_attr(target_lines, "has_multiple_abbreviation") <- "TRUE"

	# Tag lines containing vernacular
	target_lines <- xml_find_all(xml_doc, "//v/ancestor::line")
	xml_attr(target_lines, "has_vernacular") <- "TRUE"	
	target_lines <- xml_find_all(xml_doc, 
		"//line[@has_vernacular]/child::v[position() > 1]/ancestor::line")
	xml_attr(target_lines, "has_multiple_vernacular") <- "TRUE"	

	# Estimate English proportion 
	# cat(str_c(Sys.time()), "Estimate English proportion\n")	
	text_minus_metadata <- str_replace_all(all_lines_text, metadata_regex, "")
	English_count_df <- 
		count_English(text_minus_metadata) %>%
		mutate(
			weight = ifelse(word_count < 3, 
				ifelse(English_count == word_count, "majority", 
					ifelse(English_count == 0, "minority", "inconclusive")),
				ifelse((English_count / word_count) > 0.5, "majority", "minority")),
			proportion = zero_if_NA(English_count / word_count)
			)

	# Now do inference:
	# cat(str_c(Sys.time()), "collate line attributes\n")	

	line_df <- mutate(line_df,
		#####
		#####  Could be good to have something that handles brackets that close across a line — to understand those two lines are grouped.
		#####
		#####  Currently if xftf isn't punctuated, it grabs too much -- maybe require than xfn can't be way shorter than the next
		#####


		line_number = row_number(),
		text_minus_metadata = text_minus_metadata,
		text_minus_example_num = str_replace(text, str_c(ex_number_regex, "\\s*"), ""),
		length_minus_metadata = str_length(text_minus_metadata),
		length_quoted = zero_if_NA(str_length(str_extract(text_minus_metadata, quote_regex))),
		line_word_length = 1 + str_count(text, "\\s+") - str_count(text, "[-=+:﹤﹥]\\s|\\s[-=+:﹤﹥]"),  # discount spaces adjacent to morph breaks
		proportion_English = English_count_df$proportion,
		weight_of_English = English_count_df$weight,
		plain_space_count = str_count(text_minus_metadata, "[^ ] [^ ]"),
		pad_space_count = str_count(text_minus_metadata, "  +|\u00A0\u00A0+"),

		is_end_body_text = !is.na(xml_attr(all_lines, "end_body_text")),
		ex_number_text = ifelse(is_end_body_text, NA, str_extract(text, ex_number_regex)),
		ex_number_head_text = str_extract(ex_number_text, ex_head_number_regex),
		ex_number_sub_text = str_extract(ex_number_text, ex_sub_number_regex),
		has_ex_number = !is.na(ex_number_text),
		has_ex_head_number = !is.na(ex_number_head_text),
		has_ex_sub_number = !is.na(ex_number_sub_text),

		distance_since_ex_number = count_T_since_F(!has_ex_number),
		distance_until_ex_number = rev(count_T_since_F(rev(!has_ex_number))),
		distance_until_end_body_text = rev(count_T_since_F(rev(!is_end_body_text))) + 1,

		is_terminating = str_detect(text, final_terminaters),
		is_continuing = str_detect(text, final_continuers),
		is_short = (length_minus_metadata < 70),
		is_short_nonterminating = is_terminating & is_short,
		is_long_continuing = is_continuing & !is_short,
		is_even_length_with_next_line = abs(line_word_length - lead(line_word_length, default = 0, 1)) < 4,
		is_shorter_than_next_line = lead(line_word_length, default = 0, 1) - line_word_length > 4,
		has_vernacular = !is.na(xml_attr(all_lines, "has_vernacular")),
		has_multi_vernacular = !is.na(xml_attr(all_lines, "has_multiple_vernacular")),
		has_abbreviation = !is.na(xml_attr(all_lines, "has_abbreviation")),
		has_multi_abbreviation = !is.na(xml_attr(all_lines, "has_multiple_abbreviation")),
		is_plain_space_dominant = (plain_space_count > pad_space_count + 3),
		is_pad_space_dominant = (pad_space_count > plain_space_count + 3),
		has_many_junctures = (str_count(text_minus_metadata, juncture_regex) - str_count(text_minus_metadata, juncture_distractors)) > 2,
		is_mostly_quoted = (2 * length_quoted > length_minus_metadata),
		is_left_quoted = str_detect(text_minus_metadata, left_quote_regex),
		has_clause_break = str_detect(text_minus_metadata, "[,;]"),
		has_xf_identifier = str_detect(text, xf_identifier),
		has_section_number = !has_ex_number & str_detect(text_minus_metadata, "^[0-9]+\\.[0-9]"),
		# start_body_text = !is.na(xml_attr(all_lines, "start_body_text")),
		# final_punctuation = str_detect(text, final_punct_regex),
		# has_initial_subhead_number = str_detect(text, "^[0-9]+\\.[0-9]+\\.[0-9]"),

		affirm_xu = (
			weight_of_English == "minority" |
			is_short_nonterminating |
			has_multi_vernacular),
		affirm_xv = (
			weight_of_English == "minority" |
			is_short_nonterminating |
			has_multi_vernacular |
			has_many_junctures),
		affirm_xg = (
			is_short_nonterminating |
			has_multi_abbreviation |
			is_pad_space_dominant |
			has_many_junctures),
		affirm_xftn = (
			weight_of_English == "majority" |
			is_long_continuing |
			is_left_quoted |
			has_xf_identifier),
		affirm_xftf = (
			weight_of_English == "majority" |
			is_short & is_terminating |
			is_left_quoted |
			is_mostly_quoted |
			has_xf_identifier),
		affirm_xh = has_ex_number,

		disqualify_xu = (
			has_section_number |
			has_multi_abbreviation |
			has_many_junctures |
			line_word_length > 5 & proportion_English > 0.75 | 
			line_word_length > 8 & weight_of_English == "majority" |
			is_long_continuing & is_plain_space_dominant & weight_of_English == "majority" |
			!is_even_length_with_next_line),
		disqualify_xv = (
			has_section_number |
			has_multi_abbreviation |
			line_word_length > 5 & proportion_English > 0.75 | 
			line_word_length > 8 & weight_of_English == "majority" |
			is_long_continuing & is_plain_space_dominant & weight_of_English == "majority" |
			!is_even_length_with_next_line),
		disqualify_xg = (
			has_section_number |
			is_long_continuing & is_plain_space_dominant & weight_of_English == "majority" |
			has_clause_break),
		disqualify_xftn = (
			has_section_number |
			has_many_junctures |
			has_multi_abbreviation |
			is_terminating |
			is_shorter_than_next_line |
			is_pad_space_dominant),
		disqualify_xftf = (
			has_section_number |
			has_many_junctures |
			has_multi_abbreviation |
			is_pad_space_dominant),
		disqualify_xh = !has_ex_number
		)

	# Exit if no example text was found
	if (nrow(filter(line_df, has_ex_number)) == 0) { 
		cat("  no example lines detected\n")
		return(xml_doc)
	}

	# Save a file that summarizes the inference steps, for
	# help tweaking the code:
	write.csv(line_df, file.path(temp_directory, "ex_detection.csv"))

	# cat(str_c(Sys.time()), "infer line functions\n")	
	
	line_df <- select(line_df,
		line_number,
		text,
		text_minus_example_num,
		starts_with("ex_number_"),
		starts_with("has_ex_"),
		starts_with("distance_until"), 
		starts_with("affirm"),
		starts_with("disqualify"))

	# Projecting forward from each line with an example number,
	# try to match the following lines' contents to a template
	# that describes the line functions in an example sentence.
	# There are many templates, so try them starting from the 
	# most expected.

	# Initialise lists of templates, ordered by expectedness

	#
	# This is doing poorly, rushing to 3-lines for 5-line e.g.s
	#

	if (expect_xh) {
		# checks xh options first
		templates_order_df <- expand.grid(
			xh = c("xh", ""), 
			ft = c("xftn,xftn,xftf", "xftn,xftf", "xftf"), 
			n_xgg = 6:1, 
			xu = if (expect_xu) c("xu", "") else c("", "xu"),
			xv = if (expect_xv) c("xv", "") else c("", "xv"),
			xg = if (expect_xg_xg) c("xg,xg", "xg") else c("xg", "xg,xg"))
	} else {
		# checks ft options first
		templates_order_df <- expand.grid(
			ft = c("xftn,xftn,xftf", "xftn,xftf", "xftf"), 
			n_xgg = 6:1, 
			xu = if (expect_xu) c("xu", "") else c("", "xu"),
			xv = if (expect_xv) c("xv", "") else c("", "xv"),
			xg = if (expect_xg_xg) c("xg,xg", "xg") else c("xg", "xg,xg"),
			xh = c("", "xh"))
	}
	templates_order_df <- 
		templates_order_df %>%
		filter(xu == "xu" | xv == "xv") %>%
		mutate(
			xggs = strrep(str_c(xu, xv, xg, "", sep = ","), n_xgg),
			template = 
				str_c(sep = ",", xh, xggs, ft) %>% 
				str_replace_all(",+", ",") %>% 
				str_replace_all("^,|,$", ""),
			n_lines = str_count(template, ",") + 1
			) %>%
		select(n_lines, template) %>%
		separate(col = template, into = str_c("line", 1:28), 
			fill = "right", remove = FALSE)

		# print(expect_xu)
		# print(expect_xv)
		# print(templates_order_df); stop()

	# Initialise a data frame to record all places where a 
	# check will need to be done, to see if addition xft groups
	# exist, which don't start with an example number.
	follow_ons <- data.frame(line_number = numeric(0), 
		core_template = character(0), stringsAsFactors = FALSE)

	# Now go through the lines that have example numbers, and try to 
	# project templates
	ex_lines <- filter(line_df, has_ex_number)$line_number
	for (l in ex_lines) {
		# Only consider templates that could fit
		dist_to_next_ex <- line_df[(l + 1),]$distance_until_ex_number
		dist_to_end <- line_df[(l + 1),]$distance_until_end_body_text
		max_window <- min(dist_to_next_ex, dist_to_end) + 1
		this_template_df <- filter(templates_order_df, max_window >= n_lines)
		if (nrow(this_template_df) == 0) { next }
		
		max_lines <- max(this_template_df$n_lines)
		last_line <- l + max_lines - 1

		# Work out which lines are disqualified or
		# not affirmed, by line function
		local_df <- 
			line_df[l:last_line, ] %>%
			mutate(
				disqual_regex = str_c(sep = "|", 
					ifelse(disqualify_xh, "xh", ""),
					ifelse(disqualify_xv, "xv", ""),
					ifelse(disqualify_xu, "xu", ""),
					ifelse(disqualify_xg, "xg", ""),
					ifelse(disqualify_xftf, "xftf", ""),
					ifelse(disqualify_xftn, "xftn", "")) %>%
					str_replace_all("\\|+", "|") %>%
					str_replace_all("^\\||\\|$", "") %>%
					str_replace("^$", "#")
			)

		# Filter out all infeasible templates
		for (i in 1:max_lines) {
			if (nrow(this_template_df) == 0) { next }
			filter_col <- this_template_df[, str_c("line", i)]
			disqual_regex <- local_df$disqual_regex[i]
			remove_lines <- is_TRUE(str_detect(filter_col, disqual_regex))
			this_template_df <- this_template_df[!remove_lines, ]
		}	
		if (nrow(this_template_df) == 0) { next }

		# Use the top-ranked template that's feasible
		# cat(str_replace_all(line_df$ex_number_text[l], "\\s+", ""))

		n_lines <- this_template_df$n_lines[1]
		template <- this_template_df$template[1]
		last_line <- l + n_lines - 1
		seek <- unlist(str_split(template, ","))

		# Label nodes by their functions
		for (j in 1:n_lines) {
			xml_name(all_lines[[l + j - 1]]) <- 
				str_replace(seek[j], "xf..", "xf")
		}

		# Delineate the <x> node with
		# xleft and xright (note that the xright could be shifted 
		# later if the example sentence is deemed to extend further).
		first_node <- all_lines[[l]]
		last_node <- all_lines[[last_line]]
		xml_add_sibling(first_node, "xleft", .where = "before",
			x_number = line_df$ex_number_head_text[l],
			x_sub_number = empty_if_NA(line_df$ex_number_sub_text[l]))
		text_node <- xml_find_all(first_node, "descendant::text()[1]")
		xml_text(text_node) <- 
			xml_text(text_node) %>%
			str_replace(str_c(ex_number_regex, "\\s*"), "")
		xml_add_sibling(last_node, "xright", .where = "after") 

		# Now: could there be free trans groups beyond this one?
		# Assume they have the same xgg structure, but with no xh
		core_template <- str_replace_all(template, "xh,|,xftn|,xftf", "")
		shortest_next_xfg <- str_count(core_template, ",") + 2
		# If there's enough space for one to exist, add it to the 
		# list of follow-ons to check
		if (max_window >= n_lines + shortest_next_xfg) { 
			next_follow_on <- nrow(follow_ons) + 1
			follow_ons <- follow_ons %>% bind_rows(
				data.frame(line_number = l + n_lines, 
					core_template = core_template,
					stringsAsFactors = FALSE))
		}
	}

	# Now do all the follow-on checks
	follow_on_lines <- follow_ons$line_number
	n_follow_on_lines <- length(follow_on_lines)

	k <- 0
	while (k < n_follow_on_lines) {
		k <- k + 1
		l <- follow_ons$line_number[k]
		# Only templates that could fit have been added to the follow_on
		# data frame
		core_template <- filter(follow_ons, line_number == l)$core_template
		core_length <- str_count(core_template, ",") + 1
		this_template_df <- data.frame(
			n_lines = core_length + 1:3,
			template = str_c(core_template, 
				c(",xftf", ",xftn,xftf", ",xftn,xftn,xftf")),
			stringsAsFactors = FALSE) %>%
			separate(col = template, into = str_c("line", 1:28), 
				fill = "right", remove = FALSE)
		max_lines <- max(this_template_df$n_lines)
		last_line <- l + max_lines - 1

		# Work out which lines are disqualified or
		# not affirmed, by line function
		local_df <- 
			line_df[l:last_line, ] %>%
			mutate(
				disqual_regex = str_c(sep = "|", 
					ifelse(disqualify_xh, "xh", ""),
					ifelse(disqualify_xv, "xv", ""),
					ifelse(disqualify_xu, "xu", ""),
					ifelse(disqualify_xg, "xg", ""),
					ifelse(disqualify_xftf, "xftf", ""),
					ifelse(disqualify_xftn, "xftn", "")) %>%
					str_replace_all("\\|+", "|") %>%
					str_replace("^\\||\\|$", "") %>%
					str_replace("^$", "#"),
				affirm_regex = str_c(sep = "|", 
					ifelse(affirm_xh, "xh", ""),
					ifelse(affirm_xv, "xv", ""),
					ifelse(affirm_xu, "xu", ""),
					ifelse(affirm_xg, "xg", ""),
					ifelse(affirm_xftf, "xftf", ""),
					ifelse(affirm_xftn, "xftn", "")) %>%
					str_replace_all("\\|+", "|") %>%
					str_replace_all("^\\||\\|$", "") %>%
					str_replace("^$", "#")
			)

		# Filter out all infeasible templates
		for (i in 1:max_lines) {
			if (nrow(this_template_df) == 0) { next }
			filter_col <- this_template_df[, str_c("line", i)]
			disqual_regex <- local_df$disqual_regex[i]
			affirm_regex <- local_df$affirm_regex[i]
			remove_lines <- 
				is_TRUE(str_detect(filter_col, disqual_regex)) | 
				is_TRUE(!str_detect(filter_col, affirm_regex))
			this_template_df <- this_template_df[!remove_lines, ]
		}	
		if (nrow(this_template_df) == 0) { next }

		# Use the top-ranked template that's feasible
		# cat(line_df$line_number[l], "\t", line_df$text[l], "\n")

		n_lines <- this_template_df$n_lines[1]
		template <- this_template_df$template[1]
		last_line <- l + n_lines - 1
		seek <- unlist(str_split(template, ","))

		# Label nodes by their functions
		for (j in 1:n_lines) {
			xml_name(all_lines[[l + j - 1]]) <- 
				str_replace(seek[j], "xf..", "xf")
		}

		# Shift the <xright> node to the newly identified
		# right edge
		first_node <- all_lines[[l]]
		last_node <- all_lines[[last_line]]
		xright_to_remove <- xml_find_all(first_node, "preceding::xright[1]")
		if (length(xright_to_remove) != 1) { stop("Didn't find <xright> where there should be one.") }
		xml_remove(xright_to_remove)
		xml_add_sibling(last_node, "xright", .where = "after") 

		# Now: could there be free trans groups beyond this one?
		# Assume they have the same xgg structure
		dist_to_next_ex <- line_df[(l + 1),]$distance_until_ex_number
		dist_to_end <- line_df[(l + 1),]$distance_until_end_body_text
		max_window <- min(dist_to_next_ex, dist_to_end) + 1
		shortest_next_xfg <- core_length + 1
		# If there's enough space for one to exist, add it to the 
		# list of follow-ons to check
		if (max_window >= n_lines + shortest_next_xfg) { 
			next_follow_on <- nrow(follow_ons) + 1
			follow_ons <- follow_ons %>% bind_rows(
				data.frame(line_number = l + n_lines, 
					core_template = core_template,
					stringsAsFactors = FALSE))
			n_follow_on_lines <- n_follow_on_lines + 1
		}
	}

	# Remove nodes below the level of xu, xv and xg:
	# <i>, <v>, <abbr> etc., which were used for classifying
	# lines and are no longer needed. Do multiple cycles to catch nested
	# nodes (can't be done simultaneously because of how
	# xml_collapse_nodes_upwards is written)
	low_nodes <- xml_find_all(xml_doc, "//xu/child::* | //xv/child::* | //xg/child::*")
	while (length(low_nodes) > 0 ) {
		xml_doc <- xml_collapse_nodes_upwards(xml_doc, low_nodes)
		low_nodes <- xml_find_all(xml_doc, "//xu/child::* | //xv/child::* | //xg/child::*")
	}

	# Put ex numbers on table rows that seem to have them
	td_nodes <- xml_find_all(xml_doc, "//layout_xml//tr/child::td[1]")
	for (n in td_nodes) {
		text_node <- xml_find_all(n, "descendant::text()[1]")
		if (length(text_node) == 0) { next }
		ex_text <- str_c(xml_text(text_node), " ") %>% str_extract(ex_number_regex)
		if (!is.na(ex_text)) {
			xml_attr(n, "x_number") <- str_extract(ex_text, ex_head_number_regex)
			xml_attr(n, "x_sub_number") <- empty_if_NA(str_extract(ex_text, ex_sub_number_regex))		
		}
	}

	# For all nodes with sub_number but missing x_number, take
	# the most recently preceding x_number
	target_nodes <- xml_find_all(xml_doc, "//*[@x_number]")
	ex_nums <- xml_attr(target_nodes, "x_number")
	for (i in which(ex_nums == "NA")) {
		x_num <- ifelse(i == 1, "??", ex_nums[i-1])
		xml_attr(target_nodes[[i]], "x_number") <- ex_nums[i] <- x_num
	}

	write_xml(xml_doc, file.path(temp_directory, "locating_complete.xml"))
	return(xml_doc)
}


# Group example lines into high-order functional units

# First clean up the relationships between <para> nodes
# and example lines.
# Then create example nodes, <x>, delete the <para>
# node structures below them, and move <page_breaks>
# outside of examples.
# Then create two kinds of lower-order heirarchical units
# below <x>:
# - <xgg> is a glossing group, containing vernacular lines
#   and their associated gloss lines.
# - <xfg> is a free-trans group, containing one or more
#   adjacent <xgg> groups followed by <xf>.
group_interlinear_text = function(xml_doc, filename_base) {
	if (is_parameter_true("lacks_numbered_examples", filename_base)) { return(xml_doc) }

	cat(str_c(Sys.time()), "parse_interlinear_text\n")
	write_xml(xml_doc, file=file.path(temp_directory, "temp4.xml"))

	# Detach any example-initial xu or xv from a <para> node
	# in which it is not left-most
	# cat(str_c(Sys.time()), "Detach paras\n")

	# Break <para> nodes immediately before & after edges
	# of <x> nodes-to-be. Then, give the new paragraphs nodes
	# thus created the same html_id as the originals from which 
	# they were split. Finally, remove empty <para> nodes
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("(<xleft[^/>]*/>)", "(<xright/>)"),
		replacement_vec = c("</para><para>\\1", "\\1</para><para>"))	
	target_nodes <- xml_find_all(xml_doc, "//para[not(@html_id)]")
	if (length(target_nodes) > 0 ) {
		for (node in target_nodes) {
			source_node <- xml_find_all(node, "preceding-sibling::para[1]")
			if (length(source_node) == 1) {
				xml_attr(node, "html_id") <- xml_attr(source_node, "html_id")
			} else {
				xml_attr(node, "html_id") <- "unknown"				
			}
		}
	}
	xml_doc <- xml_str_replace_all(xml_doc, "<para[^/>]*>(\n *)?</para>", "")		

	# Ensure that <xleft/> and <xright/> appear outside, not
	# inside of <para> and <page_start> nodes; and change them to <x> and </x>
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = 
			c("(<para[^/>]*>)(\n *)?<xleft([^/>]*)/>",
				"(<para[^/>]*>)(\n *)?(<page_start[^/>]*/>)(\n *)?<xleft([^/>]*)/>",
				"<xright/>(\n *)?(</para>)"),
		replacement_vec = 
			c("<x\\3>\\2\\1",
				"\\1\\2\\3\\4</para><x\\5>\\2\\1",
				"\\2\\1</x>"))

	# Copy to the <x> node, all html_id's of its descendant <para> nodes
	# then remove those nodes
	# cat(str_c(Sys.time()), "Copy html_ids\n")
	x_nodes <- xml_find_all(xml_doc, "//x")
	if (length(x_nodes) == 0) { return(xml_doc) }
	for (n in x_nodes) {
		ids <- xml_attr(xml_find_all(n, "child::para"), "html_id") %>%
			str_c(collapse="+")
		xml_attr(n, "html_id") <- ids
	}
	target_nodes <- xml_find_all(xml_doc, "//x/para")
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, target_nodes)

	# If there's a <page_start> inside any <x>, move it outside, after it
	target_nodes <- xml_find_all(xml_doc, "//x/descendant::page_start")
	if (length(target_nodes) > 0) {
		for (old_node in target_nodes) {
			page_number <- xml_attr(old_node, "page_number")
			ancestor_x <- xml_find_all(old_node, "ancestor::x")
			xml_add_sibling(ancestor_x, "page_start", .where = "after")
			new_node <- xml_find_all(ancestor_x, "following-sibling::page_start[1]")
			xml_attr(new_node, "page_number") <- page_number
			xml_remove(old_node)
		}
	}

	# Group adjacent <xu>, <xv>, <xg> nodes into a "glossing group" node, <xgg>
	# cat(str_c(Sys.time()), "xgg nodes\n")
	
	# <xg> followed immediately within <x> by a not-<xg>
	target_nodes <- xml_find_all(xml_doc, "//x/child::*[not(self::xg)]/preceding-sibling::*[1]/self::xg") 
	if (length(target_nodes) > 0) { xml_add_sibling(target_nodes, "xggright", .where = "after") }
	# any <xv> node immediately preceded within <x> by a not-<xu> child of <x>
	target_nodes <- xml_find_all(xml_doc, "//x/child::*[not(self::xu)]/following-sibling::*[1]/self::xv") 
	if (length(target_nodes) > 0) { xml_add_sibling(target_nodes, "xggleft", .where = "before") }
	# any initial <xv> node in <x>
	target_nodes <- xml_find_all(xml_doc, "//x/child::*[1]/self::xv") 
	if (length(target_nodes) > 0) { xml_add_sibling(target_nodes, "xggleft", .where = "before") }
	# any <xu> node
	target_nodes <- xml_find_all(xml_doc, "//xu") 
	if (length(target_nodes) > 0) { xml_add_sibling(target_nodes, "xggleft", .where = "before") }

	# # this is to help with debugging
	# xml_assign_consecutive_ids(xml_find_all(xml_doc, "//xggleft"))  
	# xml_assign_consecutive_ids(xml_find_all(xml_doc, "//xggright"))
	# write_xml(xml_doc, file.path(temp_directory, "pre_xgg.xml"))
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("<xggleft[^/>]*/>", "<xggright[^/>]*/>"),
		replacement_vec = c("<xgg>", "</xgg>"))

	# Group one or more <xgg>, plus following <xf> into a free-trans group <xfg>
	# cat(str_c(Sys.time()), "xfg nodes\n")

	# the first <xgg> in <x>
	target_nodes <- xml_find_all(xml_doc, "//x/child::xgg[1]")
	if (length(target_nodes) > 0) { xml_add_sibling(target_nodes, "xfgleft", .where = "before") }
	# any <xf> that's followed immediately within <x> by <xgg>
	target_nodes <- xml_find_all(xml_doc, "//x/child::xgg/preceding-sibling::*[1]/self::xf")
	if (length(target_nodes) > 0) { 
		xml_add_sibling(target_nodes, "xfgleft", .where = "after") 
		xml_add_sibling(target_nodes, "xfgright", .where = "after") 
	}
	# any node that's final in <x>
	target_nodes <- xml_find_all(xml_doc, "//x/child::*[last()]")
	if (length(target_nodes) > 0) { xml_add_sibling(target_nodes, "xfgright", .where = "after") }
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("<xfgleft[^/>]*/>", "<xfgright[^/>]*/>"),
		replacement_vec = c("<xfg>", "</xfg>"))

	write_xml(xml_doc, file.path(temp_directory, "grouping_complete.xml"))
	return(xml_doc)
}

# Analyse IGT, and perform limited OCR repair

# Rejoin words with a space inserted before
# or after a morph break, e.g. "ab- cde" > "ab-cde".
# Place words within <xw> nodes. 
# Identify vernacular-gloss line pairs where the word 
# count doesn't match; attempt to fix this by:
# - finding multi-word glosses, e.g. "come out", by 
#   comparing potential ones with phrases in Wordnet.
# - finding cases where a space is inserted after a
#   numeral 1,2,3.
analyse_interlinear_text = function(xml_doc, filename_base) {
	if (is_parameter_true("lacks_numbered_examples", filename_base)) { return(xml_doc) }
	# cat(str_c(Sys.time()), "analyse_interlinear_text\n")

	# Repair words that are split at morph junctures
	# cat(str_c(Sys.time()), "Repair splits at morph junctures\n")
	morpho_parsed_nodes <- xml_find_all(xml_doc, "//xv | //xg")
	text_nodes <- xml_find_all(morpho_parsed_nodes, "descendant::text()")
	node_text <- xml_text(text_nodes)
	junct_left_regex <- "[ \u00A0]+([-=+\uFE64\uFE65.])" 
	junct_right_regex <- "([-=+\uFE64\uFE65.])[ \u00A0]+" 
	junct_left <- which(str_detect(node_text, junct_left_regex))
	junct_right <- which(str_detect(node_text, junct_right_regex))
	if(length(junct_left) > 0) {
		target_nodes <- text_nodes[junct_left]
		xml_text(target_nodes) <- 
			str_replace(xml_text(target_nodes), junct_left_regex, "\\1")
	}
	if(length(junct_right) > 0) {
		target_nodes <- text_nodes[junct_right]
		xml_text(target_nodes) <- 
			str_replace(xml_text(target_nodes), junct_right_regex, "\\1")
	}

	# Create word nodes <xw> and count them
	# cat(str_c(Sys.time()), "Create and count xw\n")
	# Remove all text-less <i/> and <v/> nodes
	xml_doc <- xml_str_replace_all(xml_doc, 
		pattern_vec = "<[iv]/>", 
		replacement_vec = "")
	# For xv, xu, xg create one big xw node within each of them
	word_parents <- xml_find_all(xml_doc, "//xu | //xv | //xg")
	xml_attr(word_parents, "word_count") <- "count_me"
	xml_doc <- xml_insert_child_layer(xml_doc, word_parents, child_name = "xw")
	# Split words 
	word_parents <- xml_find_all(xml_doc, "//*[@word_count]")
	text_nodes <- xml_find_all(word_parents, "child::xw/child::text()")
	xml_text(text_nodes) <- 
		str_replace_all(xml_text(text_nodes), "[ \u00A0]+", "♕")
	xml_doc <- xml_str_replace_all(xml_doc, 
		pattern_vec = "♕", 
		replacement_vec = "</xw><xw>")
	# Remove all text-less <xw/> nodes
	xml_doc <- xml_str_replace_all(xml_doc, 
		pattern_vec = "<xw/>", 
		replacement_vec = "")
	# Count them & mark <xgg> groups whose lines contain differing numbers of words
	count_xw_nodes(xml_doc)
	mark_word_misalignment(xml_doc)

	# Mark misalignments that are due to a long gloss
	# cat(str_c(Sys.time()), "Identify overlong glosses\n")
	misaligned_xgg <- xml_find_all(xml_doc, "//*[@word_misalignment]")
	if (length(misaligned_xgg) > 0) {
		for (xgg in misaligned_xgg) {
			xu_node <- xml_find_all(xgg, "child::xu")
			xv_node <- xml_find_all(xgg, "child::xv")
			xg_node1 <- xml_find_all(xgg, "child::xg[1]")
			if (length(xg_node1) == 0) { next }
			uv_wd_count <- max(
				ifelse(
					length(xu_node) > 0,
					xml_attr(xu_node, "word_count"), -1),
				ifelse(
					length(xv_node) > 0,
					xml_attr(xv_node, "word_count"), -1))
			xg_wd_count <- xml_attr(xg_node1, "word_count")
			if (uv_wd_count > 0 & xg_wd_count > uv_wd_count) {
				xml_attr(xgg, "long_gloss") <- "TRUE"
			}
		}
	}

	# Attempt to fix misalignments due to multi-word glosses of
	# single morphs
	# cat(str_c(Sys.time()), "Rejoin multi-word glosses\n")
	misaligned_xgg <- xml_find_all(xml_doc, "//*[@long_gloss='TRUE']")
	wordnet_multiwd <- read.csv("Wordnet_two-word_lexemes.csv")$Lexeme
	if (length(misaligned_xgg) > 0) {
		for (xgg in misaligned_xgg) {
			xg_node1 <- xml_find_all(xgg, "child::xg[1]")
			xg_words <- xml_find_all(xg_node1, "child::xw")
			if (length(xg_words) > 1) {
				for (i in 1:(length(xg_words) - 1)) {
					string_to_check <- str_c(
						str_extract(xml_text(xg_words[[i]]), "[a-z]+$"), "_",
						str_extract(xml_text(xg_words[[i+1]]), "^[a-z]+"))
					if (string_to_check %in% wordnet_multiwd) {
						target_node <- 
							xml_find_all(xg_words[[i]], "child::text()[last()]")
						if(length(target_node) > 0) {
							xml_text(target_node) <- str_c(xml_text(target_node), "_♕")
						}
					} 
				}
			}	
		}
		xml_doc <- merge_xw_nodes(xml_doc, recheck = TRUE)
	}

	# Attempt to fix misalignments due to splitting of glosses
	# after 1,2,3
	# cat(str_c(Sys.time()), "Rejoin glosses split after 123\n")
	### THIS NEEDS TO BE TWEAKED, TO CHECK THE NUMBER
	### OF JUNCTURES WOULD LINE UP
	misaligned_xgg <- xml_find_all(xml_doc, "//*[@long_gloss='TRUE']")
	if (length(misaligned_xgg) > 0) {
		for (xgg in misaligned_xgg) {
			xg_node1 <- xml_find_all(xgg, "child::xg[1]")
			xg_words <- xml_find_all(xg_node1, "child::xw[position() != last()]")
			word_ending_123 <- which(str_detect(xml_text(xg_words), "[123]$"))
			if (length(word_ending_123) > 0) {
				target_node <- 
					xml_find_all(xg_words[word_ending_123], "descendant::text()[last()]")
				if (length(target_node) > 0) {
					xml_text(target_node) <- str_c(xml_text(target_node), "♕")
				}
			}
		}
		xml_doc <- merge_xw_nodes(xml_doc, recheck = TRUE)
	}

	# Assign ids to all xgg nodes, to allow them to be
	# human-edited and updated later
	xml_find_all(xml_doc, "//xgg") %>% 
		xml_assign_consecutive_ids(attr_name = "xgg_id")

	return(xml_doc)
}

analyse_morphemes = function(xml_doc, filename_base) {
	# Mark words whose morpheme structure appears not to align
	# with the gloss
	# cat(str_c(Sys.time()), "analyse_morphemes\n")
	exx <- xml_find_all(xml_doc, "//xv/ancestor::x")
	if (length(exx) == 0) { return(xml_doc) }
	junct_regex <- "[-=+\uFE64\uFE65]" # Don't include '.', since these aren't expected to match across lines; the other delimiters stated here are 

	morpheme_df <- NULL

	# Loop through example sentences
	for (ex in exx) {
		ex_num <- xml_attr(ex, "x_number")
		ex_subnum <- xml_attr(ex, "x_sub_number")
		num_text <- 
			ifelse(is.na(ex_subnum), ex_num, 
				str_c(ex_num, "-", ex_subnum))
		xggs <- xml_find_all(ex, "descendant::xgg")
		is_aligned_xgg <- is.na(xml_attr(xggs, "word_misalignment"))
		n_aligned <- sum(is_aligned_xgg)
		if (n_aligned == 0) { next }
		xggs <- xggs[is_aligned_xgg]

		# Loop through xgg groups
		for (i in 1:n_aligned) {
			xgg <- xggs[i]
			wv <- xml_find_all(xgg, "descendant::xv/xw")
			wg <- xml_find_all(xgg, "descendant::xg[1]/xw")
			msg_id <- str_c(num_text, " <xgg> ", i)
			if (length(wv) != length(wg)) { 
				warning(str_c(filename_base, " - Unexpected misalignment in ", msg_id))
				xml_attr(xgg, "word_misalignment") <- "TRUE"
				next
			}
			if (length(wv) == 0) { 
				warning(str_c(filename_base, " - No words in ", msg_id))
				next 
			}

			# For each word, get text and junctures
			word_text <- xml_text(wv) %>% 
				str_replace("[!?.,;]$", "") %>%  # Ignore final punctuation
				str_to_lower()
			gloss_text <- xml_text(wg)
			word_junct_string <- 
				str_extract_all(word_text, junct_regex) %>%
				sapply(function(x) str_c(c(x, ""), collapse = ""))
			gloss_junct_string <- 
				str_extract_all(gloss_text, junct_regex) %>%
				sapply(function(x) str_c(c(x, ""), collapse = ""))
			is_aligned <- word_junct_string == gloss_junct_string

			# Tag misaligned words
			for (j in which(!is_aligned)) {
				xml_attr(wv[[j]], "morph_misalignment") <- "TRUE"
				xml_attr(wv[[j]], "xv_junctures") <- word_junct_string[j]
				xml_attr(wv[[j]], "xg_junctures") <- gloss_junct_string[j]	
			}

			# Tabulate morphs in aligned words
			if (!any(is_aligned)) { next }
			morphs_by_word <- str_split(word_text[is_aligned], junct_regex) # A word-by-word list of morph vectors
			glosses_by_word <- str_split(gloss_text[is_aligned], junct_regex)
			morphs_per_word <- sapply(morphs_by_word, length)
			n_words <- sum(is_aligned)
			morpheme_df <- 
				data.frame(
					ex_number = num_text,
					xgg_number = i,
					word_number = rep(1:n_words, times = morphs_per_word),
					morph = unlist(morphs_by_word),
					gloss = unlist(glosses_by_word),
					stringsAsFactors = FALSE) %>%
				bind_rows(morpheme_df, .)		
		}
	}

	if (is.null(morpheme_df)) {
		if (as.logical(get_parameter("expect_parsed_vernacular_lines", filename_base))) {
			warning(str_c(filename_base, " - Expected parsed veracular, but no morphemes found."))
		}
		return(xml_doc)
	}

	morpheme_df <- morpheme_df %>%
		group_by(ex_number, xgg_number, word_number) %>%
		mutate(
			prev_morph = str_c("/", lag(morph, 1, default = "#"), "__"),
			next_morph = str_c("/__", lead(morph, 1, default = "#")),
			prev_gloss = str_c("/", lag(gloss, 1, default = "#"), "__"),
			next_gloss = str_c("/__", lead(gloss, 1, default = "#"))
			) %>%
		ungroup() %>%
		select(-word_number, -xgg_number)	

	outfile_m <- file.path(reports_directory, 
		str_c(filename_base, "_morph_report.txt"))
	outfile_g <- file.path(reports_directory, 
		str_c(filename_base, "_gloss_report.txt"))
	write.table(
		morpheme_df %>%
			arrange(morph, gloss, prev_morph, next_morph, ex_number) %>%
			select(morph, gloss, prev_morph, prev_gloss, next_morph, next_gloss, ex_number),
		file = outfile_m, sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
	write.table(
		morpheme_df %>%
			arrange(gloss, morph, prev_gloss, next_gloss, ex_number) %>%
			select(gloss, morph, prev_gloss, prev_morph, next_gloss, next_morph, ex_number),
		file = outfile_g, sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

	return(xml_doc)
}


merge_paras_across_pages = function(xml_doc) {
	# Merge two paras on either side of a page break
	xml_doc <- 
		xml_str_replace_all(xml_doc,  
			pattern_vec = "</para>(\\n *)?<para[^/>]*>(\\n *)?<page_start",
			replacement_vec = "<page_start")  
}

# Restructure constituents of <para> nodes, from
# lines to sentences.
dissolve_lines_add_sentence_nodes = function(xml_doc) {
	cat(str_c(Sys.time()), "dissolve_lines_and_add_sentence_nodes\n")
	
	# Fix poor sentence-boundary OCR
	in_line_text_nodes <- xml_find_all(xml_doc, "//para/line/child::text()")
	node_text <- xml_text(in_line_text_nodes)
	target_indices <- which(str_detect(node_text, "[A-Za-z]{2,}\\.([AI]|[A-Z][a-z])"))
	if (length(target_indices) > 0) {
		df <- data.frame(index = target_indices) %>%
			mutate(text = str_extract_all(node_text[index], "[A-Za-z]+\\.[A-Z][a-z]*")) %>%
			unnest() %>%
			mutate(
				test_text = str_replace(text, "\\.", ". "),
				to_change = (count_English(test_text)$English_count == 2)
				)
		write.csv(df, file.path(temp_directory, "sentence_split.csv"))
		df <-	filter(df, to_change)
		if (nrow(df) > 0) {
			for (i in 1:nrow(df)) {
				index <- df$index[i]
				xml_text(in_line_text_nodes[[index]]) <- 
					str_replace_all(node_text[index], 
						fixed(df$text[index]), fixed(df$test_text[index]))
			}
		}
	}

	# Insert a marker at each sentence end
	in_line_text_nodes <- xml_find_all(xml_doc, "//para/line/child::text()")
	altered_text <- 
		xml_text(in_line_text_nodes) %>%
		str_replace_all("([A-Za-z\'\"”’\\)][!?.])\\s+([A-Z])", "\\1♔\\2") %>%
		str_replace_all("([A-Za-z\'\"”’\\)][!?.])\\s+$", "\\1♔")
	xml_text(in_line_text_nodes) <- altered_text
	# Remove line nodes, collasing them upwards. Start by
	# adding a space at the end so words don't merge across
	# line breaks.
	last_texts <- xml_find_all(xml_doc, "//line/child::text()[last()]")
	xml_text(last_texts) <- str_c(xml_text(last_texts), " ")
	target_nodes <- xml_find_all(xml_doc, "//para/line")
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, target_nodes)
	# Insert a <sentence> child below each <para>
	# and merge words fragment separated by ¬
	target_nodes <- xml_find_all(xml_doc, "//para")
	# xml_text(target_nodes) <- str_replace(xml_text(target_nodes), "¬ ", "")
	xml_doc <- xml_insert_child_layer(xml_doc, target_nodes, "sentence")
	# Convert the marker into </sentence><sentence>
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c("♔", "¬ "),
		replacement_vec = c("</sentence><sentence>", ""))
	return(xml_doc)
}

assign_id_to_all = function(xml_doc) {
	target_nodes <- xml_find_all(xml_doc, 
		str_c(c(
			"//layout_xml/descendant::para", 
			"//layout_xml/descendant::sentence",
			"//layout_xml/descendant::x",
			"//layout_xml/descendant::table",
			"//layout_xml/descendant::tr", 
			"//layout_xml/descendant::td"),
			collapse = " | "))
	cat(str_c(Sys.time()), "Assigning ids to", length(target_nodes), " nodes\n")
	xml_assign_consecutive_ids(target_nodes)
	return(xml_doc)
}


## XSLT

# Create a human-readable version
transform_augmented_xml_to_html = function(xml_file, html_outfile) {
	# cat(str_c(Sys.time()), "XSLT\n")
	system(str_c("xsltproc -o ",
		html_outfile, " grammar_augmented_markup.xsl ", xml_file))
}

# This one is used only for bug checking
transform_layout_xml_to_html = function(xml_file, html_outfile) {	
	system(str_c("xsltproc -o ",
		html_outfile, " grammar_layout_markup.xsl ", xml_file))
}


## Reports

report_examples_all = function() {
	for (f in get_all_filename_bases()) {
		cat("====", f, "====\n")
		report_examples(f)
	}
}

# Compile a report on examples detected in the grammar

# Report is written to a tab-delimited text file,
# filename_base + "_example_report.txt, formatted for
# easy, quick reading.
# Examples are searched for, both in the main text
# of the grammar (which have now been placed under <x>
# nodes) and also in tables; any found area reported
# accordingly as "text" or "table".
# Reported examples are ordered by example number
# (head number and subnumber) and any apparent gaps
# are flagged with "**".
report_examples = function(filename_base) {
	xml_doc <- read_xml(
		file.path(
			processed_xml_directory, 
			str_c(filename_base, ".xml") %>% 
				str_replace("_body_", "_augmented_")))
	outfile <- file.path(reports_directory, 
		str_c(filename_base, "_example_report.txt"))

	# Get all table and <x> nodes
	numbered_nodes <- xml_find_all(xml_doc, "//*[@x_number]")

	# Prepare regex etc. for reading example numbers
	letter_vec <- rep(1:26, 2)
	names(letter_vec) <- c(letters, LETTERS)
	roman_vec <- rep(1:49, 2)
	names(roman_vec) <- c(as.roman(1:49), str_to_lower(as.roman(1:49)))
	ex_number_regex <- get_parameter("ex_number_regex", filename_base)
	if (!nzchar(ex_number_regex)) { ex_number_regex <- "♔" }  # This is to circumvent problem that pattern="" can result in false matches
	ex_head_number_regex <- get_parameter("ex_head_number_regex", filename_base)
	ex_sub_number_regex <- get_parameter("ex_sub_number_regex", filename_base)
	if (!nzchar(ex_sub_number_regex)) { ex_sub_number_regex <- "♔" }  # This is to circumvent problem that pattern="" can result in false matches
	use_roman <- str_detect("iI", ex_sub_number_regex) & !str_detect("aA", ex_sub_number_regex)

	# Set up a data frame exx_df, with columns ex_number, sub_number,
	# id, and kind (text / table)
	exx_df <- data.frame(
			ex_number = str_trim(xml_attr(numbered_nodes, "x_number")),
			ex_subnumber = str_trim(xml_attr(numbered_nodes, "x_sub_number")),
			stringsAsFactors = FALSE) %>%
		mutate(
			id = xml_find_all(numbered_nodes, "@id") %>% xml_integer(),
			kind = ifelse(xml_name(numbered_nodes) == "td", "table", "text")
			) %>%
		filter(!is.na(ex_number) | !is.na(ex_subnumber)) %>% 
		arrange(id)

	# Infer the ordering of examples
	exx_df <- 
		mutate(exx_df,
			ex_order = as.numeric(str_extract(ex_number, "[0-9]+$")),
			subex_order = 
				ifelse(is.na(ex_subnumber), 0,
				ifelse(str_detect(ex_subnumber, "^[0-9]+$"), as.numeric(ex_subnumber),
				ifelse(str_detect(ex_subnumber, "^[ivxIVX]+$") & use_roman, roman_vec[ex_subnumber],
				ifelse(str_detect(ex_subnumber, "^[A-Za-z]$"), letter_vec[str_to_lower(ex_subnumber)], NA)))),
			full_ex_number = ifelse(is.na(ex_subnumber),
				ex_number, str_c(ex_number, "-", ex_subnumber))
			)

	# Infer missing examples from the ordering
	exx_df <- exx_df %>%
		mutate(
			diff = ex_order - lag(ex_order, default = 0),  # Difference to previous line's head_ex_number
			subdiff = subex_order - lag(subex_order, default = 0),  # Difference to next line's sub_ex_number
			seq_flag = 
				(diff == 0 & subdiff != 1) |   # Same head_number but without a simple increment in sub_number
				(diff == 1 & subex_order > 1) |   # New head_number but subnumber starts after "a"
				(kind == "text " & subex_order == 1 & lead(subex_order, default = 0) != 2) |  # subnumber = "a" not followed by "b" (but not triggered in tables)
				(diff > 1),   # Different head_number but not a simple increment 
			kind = str_c(
				ifelse(is.na(seq_flag), str_c(kind, " **"), 
				ifelse(seq_flag, str_c(kind, " **"), str_c(kind, "   ")))
				),
			id = str_c("#", id)
			) %>%
		select(id, full_ex_number, kind)

	# Write to text file

	write.table(exx_df, file = outfile,
			sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
}


## Extract example sentences for manual edits
extract_examples_all = function() {
	for (f in get_all_filename_bases()) {
		cat("====", f, "====\n")
		try(extract_examples(f))
	}
}

extract_examples = function(filename_base, only_misaligned = TRUE) {
	xml_doc <- read_xml(
		file.path(
			processed_xml_directory, 
			str_c(filename_base, ".xml") %>% 
				str_replace("_body_", "_augmented_")))
	outfile <- file.path("grammars_examples2edit", 
		str_c(filename_base, "_examples2edit.xml"))

	if (only_misaligned) {
		# Strip away non-problematic <xgg>
		word_nodes <- xml_find_all(xml_doc, "//xw")
		bad_junct_words <- word_nodes[
			xml_has_attr(word_nodes, "xg_junctures") |
			xml_has_attr(word_nodes, "xv_junctures")]
		bad_junct_xgg <- xml_find_all(bad_junct_words, "ancestor::xgg")
		xml_attr(bad_junct_xgg, "word_misalignment") <- "TRUE"
		xgg_nodes <- xml_find_all(xml_doc, "//xgg")
		xml_remove(xgg_nodes[!xml_has_attr(xgg_nodes, "word_misalignment")])
	}

	# Remove attributes
	word_nodes <- xml_find_all(xml_doc, "//xw")
	parent_nodes <- xml_find_all(xml_doc, "//xu | //xv | //xg")
	xgg_nodes <- xml_find_all(xml_doc, "//xgg")
	for (n in word_nodes) {
		xml_attrs(n) <- NULL # rep("", length(xml_attrs(n)))
	}
	for (n in parent_nodes) {
		xml_attrs(n) <- NULL # rep("", length(xml_attrs(n)))
	}
	for (n in xgg_nodes) {
		xml_attrs(n) <- NULL # rep("", length(xml_attrs(n)))
	}

	# xml_attr(word_nodes, "morph_misalignment") <- ""
	# xml_attr(word_nodes, "xg_junctures") <- ""
	# xml_attr(word_nodes, "xv_junctures") <- ""
	# xml_attr(word_parents, "line_id") <- ""
	# xml_attr(word_parents, "word_count") <- ""
	# xml_attr(xgg_nodes, "word_misalignment") <- ""
	# xml_attr(xgg_nodes, "long_gloss") <- ""

	# Copy x_numbers to xgg
	for (xgg in xgg_nodes) {
		x_node <- xml_find_all(xgg, "ancestor::x")
		ex_num <- xml_attr(x_node, "x_number")
		ex_subnum <- xml_attr(x_node, "x_sub_number")
		xml_attr(xgg, "ex_number") <- 
			ifelse(is.na(ex_subnum), ex_num, 
					str_c(ex_num, "-", ex_subnum)) 
	}

	# Strip away everything bar <xgg> nodes
	xml_remove_node_type(xml_doc,
		c("Abbyy_html", "para", "table", "img",
			"page_start", "xh", "xf", "h1",
			"h2", "h3", "h4", "h5", "h6", "a"))
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, xml_find_all(xml_doc, "//layout_xml"))
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, xml_find_all(xml_doc, "//xfg"))
	xml_doc <- xml_collapse_nodes_upwards(xml_doc, xml_find_all(xml_doc, "//x"))

	# Call the root "xgg_editor" and save the XML
	root_node <- xml_root(xml_doc)
	xml_name(root_node) <- "xgg_editor"
	write_xml(xml_doc, outfile)
}


## File preparation

create_default_param_files = function() {
	# Create default parameter files for new
	# Abbyy html files
	filename_bases <- get_new_filename_bases()
	path_base <- preprocessed_directory
	template_file <- "_template_params.txt"
	i <- 0
	for (f in filename_bases) {
		out_file <- str_c(f, "_params.txt")
		out_path <- file.path(path_base, out_file)
		if (file.exists(out_path)) {
			cat(out_file, "already exists; no changes made to it.\n")
		} else {
			template_path <- file.path(path_base, template_file)	
			file.copy(from = template_path, to = out_path)
			i <- i + 1
		}
	}
	cat(i, "files created.\n")
}


## Minor functions

get_all_filename_bases = function() {
	list.files(
		path = preprocessed_directory, 
		pattern = "_body_.*\\.htm") %>%
	str_replace("\\.html?$", "")
}

get_new_filename_bases = function() {
	all_files <- get_all_filename_bases()
	old_files <- 
		list.files(path = processed_xml_directory) %>%
		str_replace("\\.xml$", "") %>%
		str_replace("_augmented_", "_body_")
	setdiff(all_files, old_files)
}

str_if = function(string, condition_vec) {
	string_vec <- rep(string, length(condition_vec))
	string_vec[!condition_vec] <- ""
	return(string_vec)
}

str_intersect = function(string_vec1, string_vec2) {
	df <- data.frame(s1 = string_vec1, s2 = string_vec2, stringsAsFactors = FALSE)
	df <- mutate(rowwise(df),
		intersect1 = ifelse(nzchar(s1) & nzchar(s2),
			intersect(unlist(str_split(s1, "")), unlist(str_split(s2, ""))) %>% 
				str_c(collapse = ""),
			""),
		intersect = ifelse(is.na(intersect1), "", intersect1))
		return(df$intersect)
}

get_parameter = function(param, filename_base) {
	param_lines <- scan(
		file.path(
			preprocessed_directory, 
			str_c(filename_base, "_params.txt")),
		what = "character", sep = "\n", 
		encoding = "UTF-8", quiet = TRUE)
	param_regex <- str_c("^", param, ":\t")
	matches <- which(str_detect(param_lines, param_regex))
	if (length(matches) > 0) {
		results <- str_replace(param_lines[matches], param_regex, "")
	} else {
		results <- ""
	}
	return(results)
}

is_parameter_true = function(param, filename_base) {
	str_detect(get_parameter(param, filename_base), "TRUE")
}

weight_of_English = function(string_vec) {
	df <- count_English(string_vec) %>%
		mutate(weight = 
			ifelse(word_count < 3, 
				ifelse(English_count == word_count, "majority", 
					ifelse(English_count == 0, "minority", "inconclusive")),
				ifelse((English_count / word_count) > 0.5, "majority", "minority")
			))
	return(df$weight)
}

proportion_English = function(string_vec) {
	df <- count_English(string_vec) %>%
		mutate(proportion = English_count / word_count,
			proportion = ifelse(is.na(proportion), 0, proportion))
	return(df$proportion)
}

# Count tokens of word types identifiable as English
#
# Given a vector of strings (string_vec), return a dataframe with
# columns:
# - word_count, which gives a count of word types in each
#   element of string_vec
# - English_count, which gives a count of the word types
#   identified as English
count_English = function(string_vec, verbose = FALSE) {
	morpho_simplified_string_vec <- simplify_English(string_vec)
	word_types <-
		morpho_simplified_string_vec %>%
		str_split("\\s+") %>% 
		unlist() %>% unique()
	English_word_types <- 
		get_English_word_list() %>%
		intersect(., word_types) %>% 
		setdiff("")
	tab <- data.frame(string = str_c(morpho_simplified_string_vec, " ")) %>% rowwise()
	tab <- mutate(tab,
			word_types = string %>% 
				str_split("\\s+") %>%
				unique(),
			word_count = length(word_types) - 1,
			English_count = length(intersect(word_types, English_word_types)))
		if (!verbose) { tab <- tab %>% select(word_count, English_count) }
	return(tab)
}

simplify_English = function(string_vec) {
	hyphen_affixes <- str_c(collapse = "|", 
		c("-level", "-final", "-initial", "-medial", "-like",
			"-reference", "tense-",
			"sub-", "inter-", "intra-", "cross-", "extra-",
			"pre-", "post-", "mid-", "self-", "proto-",
			"morpho-", "semi-", "trans-", "non-"))
	phonemes_regex <- str_c("/.{1,2}/")
	string_vec %>%
		str_trim() %>%
		str_c(" ") %>%
		str_replace_all("\\s+", " ") %>%
		str_replace_all(hyphen_affixes, "") %>%
		str_replace_all(phonemes_regex, "") %>%
		str_replace_all("([^.] )[A-Z][a-z]+", "\\1") %>%  # ignore names
		str_replace_all("(\\(|\\[)?[0-9]+(\\.[0-9])?%?(\\)|\\])?", "") %>%  # ignore numbers
		str_replace_all(" [A-Z]{2,} ", " ") %>% # ignore acronyms
		str_to_lower() %>%
		str_replace_all("[/~]", " ") %>% 
		str_replace_all("n(’|')t", "") %>% 
		str_replace_all("(’|')(s|d|ll|ve|m|re) ", " ") %>% 
		str_replace_all("[.,!?:;‘’“”(){}\\[\\]]", "") %>% 
		str_replace_all("(^| )vs\\.? ", "versus ") %>% 
		str_replace(" +$", "")
}

get_English_word_list = function() {
	English_word_list
	## Here's how it was assembled:
	# words <- wordnet_data %>%
	# 	filter(!str_detect(Word, "[0-9]"))
	# nouns <- filter(words, POS == "n")$Word
	# verbs <- filter(words, POS == "v")$Word
	# other <- filter(words, POS == "r" | POS == "a")$Word
	# nouns <- c(nouns, suffix_s(nouns))
	# verbs <- c(verbs, suffix_s(verbs), suffix_ed(verbs), suffix_ing(verbs))
	# stop_words <- c("a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "cannot", "could", "did", "do", "does", "doing", "down", "during", "each", "few", "for", "from", "further", "had", "has", "have", "having", "he", "her", "here", "hers", "herself", "him", "himself", "his", "how", "i", "if", "in", "into", "is", "it", "its", "itself", "me", "more", "most", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own", "same", "she", "should", "so", "some", "such", "than", "that", "the", "their", "theirs", "them", "themselves", "then", "there", "these", "they", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "with", "would", "you", "your", "yours", "yourself", "yourselves")
	# irregular_pasts <- c("arisen", "arose", "ate", "awoke", "awoken", "backslid", "bade", "beat", "beaten", "became", "become", "been", "began", "begun", "bent", "bidden", "bit", "bitten", "bled", "blew", "blown", "bore", "born", "borne", "bought", "bound", "bred", "broke", "broken", "brought", "browbeaten", "built", "burnt", "burst", "bust", "came", "cast", "caught", "chose", "chosen", "clad", "clung", "come", "cost", "crept", "crossbred", "cut", "daydreamt", "dealt", "did", "disproven", "done", "dove", "drank", "drawn", "dreamt", "drew", "driven", "drove", "drunk", "dug", "dwelt", "eaten", "fallen", "fed", "fell", "felt", "fit", "fled", "flew", "flown", "flung", "forbade", "forbidden", "forecast", "foregone", "foresaw", "foreseen", "foretold", "forewent", "forgave", "forgiven", "forgot", "forgotten", "forsaken", "forsook", "fought", "found", "frostbit", "frostbitten", "froze", "frozen", "gave", "given", "gone", "got", "gotten", "grew", "ground", "grown", "had", "hand-fed", "handwritten", "handwrote", "heard", "held", "hewn", "hid", "hidden", "hit", "hung", "hurt", "inbred", "inlaid", "input", "interbred", "interwound", "interwove", "interwoven", "jerry-built", "kept", "knelt", "knew", "knit", "known", "laid", "lain", "lay", "leant", "leapt", "learnt", "led", "left", "lent", "let", "lip-read", "lit", "lost", "made", "meant", "met", "miscast", "misdealt", "misdid", "misdone", "misheard", "mislaid", "mislearnt", "misled", "misread", "misset", "misspelt", "misspent", "misspoke", "misspoken", "mistaken", "mistaught", "mistook", "misunderstood", "miswritten", "miswrote", "mown", "offset", "outbid", "outbred", "outdid", "outdone", "outdrank", "outdrawn", "outdrew", "outdriven", "outdrove", "outdrunk", "outflew", "outflown", "outfought", "outgrew", "outgrown", "outleapt", "outran", "outridden", "outrode", "outrun", "outsang", "outsat", "outshone", "outshot", "outslept", "outsold", "outsped", "outspent", "outspoke", "outspoken", "outsung", "outswam", "outswore", "outsworn", "outswum", "outthought", "outthrew", "outthrown", "outwritten", "outwrote", "overate", "overbid", "overbought", "overbred", "overbuilt", "overcame", "overcome", "overdid", "overdone", "overdrank", "overdrawn", "overdrew", "overdrunk", "overeaten", "overfed", "overheard", "overhung", "overlaid", "overpaid", "overran", "overridden", "overrode", "overrun", "oversaw", "overseen", "oversewn", "overshot", "overslept", "oversold", "overspent", "overspilt", "overspoke", "overspoken", "overtaken", "overthought", "overthrew", "overthrown", "overtook", "overwound", "overwritten", "overwrote", "paid", "partaken", "partook", "pled", "prebuilt", "predid", "predone", "premade", "prepaid", "preset", "preshrank", "preshrunk", "presold", "proofread", "proven", "put", "quick-froze", "quick-frozen", "quit", "ran", "rang", "reawaken", "reawoke", "rebid", "rebound", "rebroadcast", "rebuilt", "recast", "recut", "redealt", "redid", "redone", "redrawn", "redrew", "refit", "regrew", "reground", "regrown", "reheard", "rehung", "reknit", "relaid", "relearnt", "relit", "remade", "repaid", "reran", "reread", "rerun", "resent", "reset", "resewn", "resold", "retaken", "retaught", "rethought", "retold", "retook", "retore", "retorn", "retread", "retrofit", "rewaken", "rewet", "rewoke", "rewon", "rewore", "reworn", "rewound", "rewove", "rewoven", "rewritten", "rewrote", "rid", "ridden", "risen", "rode", "rose", "roughcast", "run", "rung", "said", "sand-cast", "sang", "sank", "sat", "saw", "sawn", "seen", "sent", "set", "sewn", "shaken", "shat", "shaven", "shone", "shook", "shorn", "shot", "shown", "shrank", "shrunk", "shut", "sight-read", "slain", "slept", "slew", "slid", "slit", "slung", "slunk", "smelt", "snuck", "sold", "sought", "sown", "spat", "sped", "spelt", "spent", "spilt", "spit", "split", "spoilt", "spoke", "spoken", "sprang", "spread", "sprung", "spun", "stank", "stole", "stolen", "stood", "strewn", "stricken", "stridden", "striven", "strode", "strove", "struck", "strung", "stuck", "stung", "stunk", "sublet", "sunburnt", "sung", "sunk", "swam", "sweat", "swept", "swollen", "swore", "sworn", "swum", "swung", "taken", "taught", "telecast", "test-driven", "test-drove", "test-flew", "test-flown", "thought", "threw", "thrown", "thrust", "told", "took", "tore", "torn", "trod", "trodden", "typecast", "typeset", "typewritten", "typewrote", "unbent", "unbound", "unclad", "underbid", "undercut", "undergone", "underlain", "underlay", "undersold", "underspent", "understood", "undertaken", "undertook", "underwent", "underwritten", "underwrote", "undid", "undone", "unfroze", "unfrozen", "unhid", "unhidden", "unhung", "unknit", "unlearnt", "unsewn", "unslung", "unspun", "unstrung", "unstuck", "unwound", "unwove", "unwoven", "upheld", "upset", "was", "waylaid", "went", "wept", "were", "wet", "withdrawn", "withdrew", "withheld", "withstood", "woke", "woken", "won", "wore", "worn", "wound", "wove", "woven", "written", "wrote", "wrung")
	# irregular_plurals <- c("feet", "geese", "lice", "men", "mice", "teeth", "women", "calve", "elves", "halves", "hooves", "knives", "leaves", "lives", "loaves", "selves", "shelves", "thieves", "wives")
	# return(c(nouns, verbs, other, stop_words, irregular_pasts, irregular_plurals))
}

suffix_s = function(string_vec) {
	string_vec %>%
 		str_replace("([sxz])$", "\\1es") %>%
 		str_replace("([sc]h)$", "\\1es") %>%
 		str_replace("y$", "ies") %>%
  	str_replace("([^s])$", "\\1s")
}

suffix_ed = function(string_vec) {
	string_vec %>% str_c("♔") %>%
 		str_replace("e♔", "ed") %>%
 		str_replace("y♔", "ied") %>%
  	str_replace("♔", "ed")
}

suffix_ing = function(string_vec) {
	string_vec %>% 
 		str_replace("e?$", "ing")
}

count_xw_nodes = function(xml_doc) {
	word_parents <- xml_find_all(xml_doc, "//*[@word_count]")
	for (wp in word_parents) {
		xml_attr(wp, "word_count") <- 
			length(xml_find_all(wp, "child::xw"))
	}
}

mark_word_misalignment = function(xml_doc) {
	xgg_nodes <- xml_find_all(xml_doc, "//xgg")
	for (xgg in xgg_nodes) {
		word_count_diversity <- 
			xml_find_all(xgg, "descendant::*/@word_count") %>% 
			  xml_text() %>% unique() %>% length()
		if (word_count_diversity > 1) {
			xml_attr(xgg, "word_misalignment") <- "TRUE"
		} else {
			xml_attr(xgg, "word_misalignment") <- ""
			xml_attr(xgg, "long_gloss") <- ""
		}
	}
}

merge_xw_nodes = function(xml_doc, recheck = TRUE) {
	# Merge <xm> node wherever the "♕" marker has been placed
	xml_doc <- xml_str_replace_all(xml_doc,
		pattern_vec = c(
			"♕</(v|i)>",  # move marker out of <v> or <i> nodes
			"♕(\n *)?</xw>(\n *)?<xw>"),  # merge <xw> with following
		replacement_vec = c("</\\1>♕", ""))
	if (recheck) {
		count_xw_nodes(xml_doc)
		mark_word_misalignment(xml_doc)
	}
	return(xml_doc)
}

count_T_since_F = function(logical_vec) {
	count_vec = as.integer(logical_vec)
	for(i in 2:length(logical_vec)) {
		count_vec[i] <- ifelse(logical_vec[i], count_vec[i-1]+1, 0)
	}
	count_vec
}

is_TRUE = function(logical_vec) {
	logical_vec[is.na(logical_vec)] <- FALSE
	logical_vec
}

zero_if_NA = function(num_vec) {
	num_vec[is.na(num_vec)] <- 0
	num_vec
}

empty_if_NA = function(string_vec) {
	string_vec[is.na(string_vec)] <- ""
	string_vec
}