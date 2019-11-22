# =================================
# Extra functions for Hadley's xml2
# =================================
# Erich Round
# Created 2017
# Last updated 2017-01-03
# =================================

library(xml2)


xml_clone_children = function(xml_doc, from_parent, to_parent) {
	# Do cloning, then tidy up text elements
	xml_clone_children_(from_parent, to_parent)
	return(xml_convert_elements_to_text_nodes(xml_doc))
}

xml_clone_children_ = function(from_parent, to_parent) {
	# WARNING
	# I've tried to implement something to stop you copying
	# a node inside itself, but can't work out how to do
	# it, so BE CAREFUL!

	children_original <- xml_contents(from_parent)
	n_children <- length(children_original)
	if (n_children == 0) { return() }

	child_filled_text_nodes <- xml_find_all(from_parent, "child::text()")
	child_filled_text_node_indices <- which(xml_name(children_original) == "text")
	child_text_nodes <- rep("", n_children)
	child_text_nodes[child_filled_text_node_indices] <- xml_text(child_filled_text_nodes)

	for (i in 1:n_children) {
		child_original <- children_original[[i]]
		child_name <- xml_name(child_original)
		child_attrs <- xml_attrs(child_original)
		child_text <- child_text_nodes[i]

		# Don't copy over non-meaningful code formatting, which
		# shows up in <text> nodes as "^\\n *$"
		if (child_name == "text" & str_detect(child_text, "^\\n *$")) {
			next
		}
		# See xlm_fix_text_nodes() for why i'm using this
		# hack to get text elements to work
		if (child_name == "text") { child_name <- "to_text_element" }

		# Add the new node and take note of which it is,
		# namely the last child of to_parent
		xml_add_child(to_parent, .value = child_name)
		child_new <- xml_child(to_parent, search = length(xml_contents(to_parent)))

		# Add attributes & text to it
		xml_attrs(child_new) <- child_attrs
		xml_text(child_new) <- child_text

		# Add children too, if the original has any
		if (length(xml_contents(child_original)) > 0) {
			xml_clone_children_(
				from_parent = child_original,
				to_parent = child_new)
		}
	}
}

xml_remove_node_type = function(xml_doc, node_type) {
	for (type in node_type) {
		nodes <- xml_find_all(xml_doc, str_c("//", type))
		if (length(nodes) > 0) { xml_remove(nodes) }
	}
	return(xml_doc)
}

xml_convert_elements_to_text_nodes = function(xml_doc) {
	# xlm2 gives you only poor control over where you can 
	# create a text node. This hack will change any element
	# named <to_text_node> into a real text node by
	# directly altering the XML document.

	xml_collapse_nodes_upwards(xml_doc, 
		nodes = xml_find_all(xml_doc, "//to_text_element"))
}

xml_make_snug = function(xml_file, type_1, type_2) {
	# Within a saved xml file, removes spaces between 
	# nodes of type_1 and type_2, which xml2 splits up
	# across lines.
	# Does so by brute force by altering the XML document.

	type_1_open <- str_c("(<", type_1, "[^>/]*>)")
	type_1_close <- str_c("(</", type_1, ">)")
	type_2_open <- str_c("(<", type_2, "[^>/]*>)")
	type_2_close <- str_c("(</", type_2, ">)")

	xml_string <- 
		xml_doc_to_xml_string(xml_doc) %>%
		str_replace_all(str_c(type_1_close, "(\\n *)?", type_2_open), "\\1\u200B\\3") %>%
		str_replace_all(str_c(type_2_close, "(\\n *)?", type_1_open), "\\1\u200B\\3")

	write(xml_string, xml_file)
}

xml_assign_consecutive_ids = function(nodes, attr_name = "id", prefix = "") {
	n_nodes <- length(nodes)
	if (n_nodes > 0) {
		for (i in 1:n_nodes) {
			xml_attr(nodes[[i]], attr_name) <- str_c(prefix, i)
		}
	}
}

xml_count_descendants = function(nodes, descendant_type) {
	n_nodes <- length(nodes)
	if (n_nodes == 0) { return(integer(0)) }
	results <- rep(0, n_nodes)
	xpath <- str_c("descendant::", descendant_type, "[last()]")
	for(n in 1:n_nodes) {
		results[n] <- xml_find_all(nodes[[n]], xpath) %>% length()
	}
	return(results)
}

xml_count_descendants2 = function(nodes, descendant_type) {
	# Attempt at a slightly fast function. It's still very slow

	n_nodes <- length(nodes)
	if (n_nodes == 0) { return(integer(0)) }
	results <- rep(0, n_nodes)
	xpath <- str_c("descendant::", descendant_type, "[last()]")
	for(n in 1:n_nodes) {
		last_desc <- xml_find_all(nodes[[n]], xpath)
		if (length(last_desc) > 0) {
			results[n] <- xml_path(last_desc) %>%
				str_replace("([^\\]])$", "[1]") %>%
				str_sub(1, -2) %>%
				str_extract("[0-9]+$") %>%
				as.integer()
		}
	}
	return(results)
}

tabbed_text_to_xml = function(filename_base, node_name = "taxonomy_node", name_space = NULL) {
	infile <- str_c(filename_base, ".txt")
	outfile <- str_c(filename_base, ".xml")
	line_vec <- scan(infile,
		what = "character", sep = "\n", 
		encoding = "UTF-8", quiet = TRUE)
	# Remove comment-only & whitespace only lines
	used_lines <- which(!str_detect(line_vec, "^\\s*[#$]"))
	line_vec <- line_vec[used_lines]
	if (is.null(name_space)) { name_space <- filename_base }

	# Initialise the tree
	xml_tree <- read_xml("<root id='0'/>")
	previous_depth <- -1
	latest_id_by_depth <- c(0)

	for (i in 1:length(line_vec)) {
		line_text <- line_vec[i]
		node_depth <- nchar(str_extract(line_text, "^\t*"))
		label_text <- str_c(name_space, "::",
			str_trim(str_replace_all(line_text, "#.*$", "")))  # remove hashed comments

		id <- str_c(i)

		if (node_depth > previous_depth + 1) {
			stop(str_c("Node skipped at line ", used_lines[i]))
		}
		xpath <-str_c("//*[@id='", latest_id_by_depth[node_depth + 1], "']")
		parent_node <- xml_find_all(xml_tree, xpath)
		xml_add_child(parent_node, node_name, id = id, label = label_text)

		# Update counters etc.
		latest_id_by_depth[node_depth + 2] <- id
		previous_depth <- node_depth
	}

	if (is.null(outfile)) {
		outfile <- str_c(filename_base, ".xml")
	}
	write_xml(xml_tree, outfile)
}


### Operations implemented by manipulating the 
### string representation of an xml_doc

html_2_xml = function(html_doc) {
	str_c(html_doc) %>%
		str_replace("<!DOCTYPE[^\n]*\n", "") %>%
		str_replace_all("(<(meta|link|br|img)[^>\n]*)>", "\\1/>") %>%
		read_xml()
}

xml_collapse_nodes_upwards = function(xml_doc, nodes) {
	# Remove these nodes and dump their content 
	# directly inside their parent

	xml_name(nodes) <- "_collapse_me_"
	xml_str_replace_all(xml_doc,
		pattern_vec = c(
			"</_collapse_me_>(\\n *)?<_collapse_me_[^>/]*>",  # close + opening
			">(\\n *)?<_collapse_me_[^>/]*>",  # opening preceded by another tag
			"</_collapse_me_>(\\n *)?<",  # close followed by another tag
			">(\\n *)?<_collapse_me_[^>/]*/>(\\n *)?<",  # unit bordered by tags
			">(\\n *)?<_collapse_me_[^>/]*/>",  # unit bordered only by left tag
			"<_collapse_me_[^>/]*/>(\\n *)?<",  # unit bordered only by right tag
			"<_collapse_me_[^>/]*>",  # opening without bordering tag
			"</_collapse_me_>",  # close without bordering tag
			"<_collapse_me_[^>/]*/>"),  # unit without bordering tags
		replacement_vec = c("", ">", "<", "><", ">", "<", "", "", ""))
}

xml_insert_parent_layer = function(xml_doc, target_nodes, parent_name) {
	# Insert a parent between a target node and its current parent
	# The function will also work if given multiple target_nodes, 
	# btu requires all target nodes to have the same name.

	orig_names <- xml_name(target_nodes)
	if (length(unique(orig_names)) > 1) {
		stop("xml_insert_parent_layer parameter target_nodes: nodes should all have the same name.")
	}

	xml_name(target_nodes) <- "_expand_me_"
	xml_str_replace_all(xml_doc,
		pattern_vec = c(
			"(<_expand_me_[^>/]*>)", 
			"(</_expand_me_>)", 
			"(<_expand_me_[^>/]*/>)", 
			"_expand_me_"),
		replacement_vec = c(
			str_c("<", parent_name, ">\\1"),  # parent opening before child opening
			str_c("\\1</", parent_name, ">"),  # parent close after child close
			str_c("<", parent_name, ">\\1</", parent_name, ">"),  # parent opening and close around unit child
			orig_names[1]))  # restore the original name of the target nodes
}

xml_insert_child_layer = function(xml_doc, target_nodes, child_name) {
	# Insert a child between a target node and its current children
	# The function will also work if given multiple target_nodes, 
	# btu requires all target nodes to have the same name.

	all_targets <- target_nodes
	if (length(all_targets) == 0) { return(xml_doc) }
	parent_names <- xml_name(all_targets) %>% unique()

	for (p in parent_names) {
		target_nodes <- xml_find_all(all_targets, str_c("self::", p))
		xml_name(target_nodes) <- str_c("_expand_me_", p, "#_")
	}

	xml_str_replace_all(xml_doc,
		pattern_vec = c(
			"(<_expand_me_[^>/]*>)", 
			"(</_expand_me_[^>]*>)", 
			"(<(_expand_me_[^ ]*)([^>/]*)/>)", 
			str_c("_expand_me_", parent_names, "#_")),
		replacement_vec = c(
			str_c("\\1<", child_name, ">"),  # child opening after parent opening
			str_c("</", child_name, ">\\1"),  # child close before parent close
			str_c("<\\2\\3><", child_name, "/></\\2>"), # unit child within previously-unit parent
			parent_names))  # restore the original name of the target nodes
}

xml_wrap_text_in_node = function(xml_doc, text_pattern, node_open, node_close) {
	# Find matching text in an xml_doc and wrap it
	# in a node by simply inserting node_open before it
	# and node_close afterwards.

	xml_str_replace_all(xml_doc,
		pattern_vec = str_c("(", text_pattern, ")"),
		replacement_vec = str_c(node_open, "\\1", node_close))
}

xml_repel = function(xml_doc, repeller, repelled) {
	# Moves daughterless <repelled> nodes outside of 
	# any <repeller> nodes which they're sitting at
	# the edge of.
	# Does so by brute force by altering the XML document.

	repeller_open <- str_c("(<", repeller, "[^>/]*>)")
	repeller_close <- str_c("(</", repeller, ">)")
	repelled_unit <- str_c("(<", repelled, "[^>/]*/>)")

	xml_str_replace_all(xml_doc, 
		pattern_vec = c(
			str_c(repeller_open, "(\\n *)?", repelled_unit),
			str_c(repelled_unit, "(\\n *)?", repeller_close)),
		replacement_vec = c("\\3\\1", "\\3\\1"))

	# xml_string <- 
	# 	xml_doc_to_xml_string(xml_doc) %>%
	# 	str_replace_all(str_c(repeller_open, "(\\n *)?", repelled_unit), "\\3\\1") %>%
	# 	str_replace_all(str_c(repelled_unit, "(\\n *)?", repeller_close), "\\3\\1")
	# return(read_xml(xml_string))
}

xml_doc_to_xml_string = function(xml_doc) {
	str_c(xml_doc)
	# Old script:
	# write_xml(xml_doc, file = file.path("temp_files", "temp_.xml"))
	# xml_string <- 
	# 	scan(file.path("temp_files", "temp_.xml"), what = "character", 
	# 		sep = "\n", encoding = "UTF-8", quiet = TRUE) %>%
	# 		str_c(collapse = "\n")
}

xml_str_replace_all = function(xml_doc, pattern_vec, replacement_vec) {
	# Do a set of str_replace_all() operations within 
	# the string representation of xml_doc

	xml_string <- xml_doc_to_xml_string(xml_doc)
	if (length(xml_string) == 0) {
		print(xml_doc)
		stop("xml_doc is empty.")
	}
	for (i in 1:length(pattern_vec)) {
		xml_string <- str_replace_all(
			xml_string, pattern_vec[i], replacement_vec[i])
	}
	write(xml_string, file.path("temp_files", "temp__.xml"))   ## Use for debugging
	return(read_xml(xml_string))
}