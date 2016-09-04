.comments <-
    function(what=c("genes", "variants", "evidence"), id)
{}

.meta_path <- function(json)
    json %>% 
        spread_values(
            current_page = jnumber("current_page"),
            per_page = jnumber("per_page"),
            total_pages = jnumber("total_pages"),
            total_count = jnumber("total_count")
        ) %>% enter_object("links") %>%
        spread_values(
            nexturl = jstring("next"),
            prevurl = jstring("previous")
        )

## genes

#' @importFrom dplyr %>% select
#' @importFrom tidyjson enter_object gather_array spread_values
#'     append_values_string jstring jnumber

.gene_path <- function(json)
    json %>%
        spread_values(
            id = jnumber("id"),
            name = jstring("name"),
            entrez_id = jstring("entrez_id"),
            description = jstring("description")
        ) %>% enter_object("aliases") %>% gather_array %>%
        append_values_string("alias") %>%
        select(id, name, entrez_id, alias, description)

.variant_path <- function(json)
    json %>%
        spread_values(id = jnumber("id")) %>%
        enter_object("variants") %>% gather_array %>%
        spread_values(
            variant_id = jnumber("id"),
            variant_name = jstring("name")
        ) %>% select(id, variant_id, variant_name)

.evidence_path <- function(json)
    json %>% spread_values(id = jnumber("id")) %>%
        enter_object("variants") %>% gather_array %>%
        spread_values(variant_id = jnumber("id")) %>%
        enter_object("evidence_items") %>%
        spread_values(
            accepted_count = jnumber("accepted_count"),
            rejected_count = jnumber("rejected_count"),
            submitted_count = jnumber("submitted_count")
        ) %>%
        select(id, variant_id, accepted_count, rejected_count, submitted_count)    
.source_path <- function(json)
    json %>%
        spread_values(
            id = jnumber("id")
        ) %>% enter_object("sources") %>% gather_array %>%
        spread_values(
            pubmed_id = jstring("pubmed_id"),
            citation = jstring("citation"),
            source_url = jstring("source_url")
        ) %>% select(id, pubmed_id, citation, source_url)

#' Accessing CIViC resources
#' 
#' @name CIViC_API
#' @importFrom httr content
#' @export
genes <-
    function(page=1, count=25)
{
    stopifnot(is_single_number(page), is_single_number(count))
    response <- .get("genes", c(page=page, count=count))

    json <- content(response, as="text")
    records <- json %>% enter_object("records") %>% gather_array
    
    meta <- .meta_path(json %>% enter_object("_meta"))
    gene_id <- .gene_path(records)
    variant <- .variant_path(records)
    evidence <- .evidence_path(records)

    src_CIViC(meta=meta, gene=gene_id, variant=variant, evidence=evidence)
}

#' @rdname CIViC_API
#' @importFrom httr content
#' @export
gene_detail <-
    function(id, identifier_type=c("civic_id", "entrez_id", "entrez_symbol"))
{
    id <- as.character(id)
    stopifnot(is_single_character(id))
    identifier_type <- match.arg(identifier_type)

    query <- paste0("genes/", id)
    response <- .get(query, identifier_type)

    json <- content(response, as="text")

    gene_id <- .gene_path(json)
    variant <- .variant_path(json)
    ## FIXME variant_groups
    evidence <- .evidence_path(json)
    sources <- .source_path(json)

    src_CIViC(gene=gene_id, variant=variant, evidence=evidence, source=sources)
}

#' @rdname CIViC_API
#' @export
genes_comments <- function(id)
    .comments("genes", id)

## variants

.variant_coodinates <- function(json)
    json %>%
        spread_values(
            chromosome = jstring("chromosome"),
            start1 = jnumber("start"),
            stop1 = jnumber("stop"),
            reference_bases = jstring("reference_bases"),
            variant_bases = jstring("variant_bases"),
            representative_transcript = jstring("representative_transcript"),
            chromosome2 = jstring("chromosome2"),
            start2 = jnumber("start2"),
            stop2 = jnumber("stop2"),
            representative_transcript2 = jstring("representative_transcript2"),
            ensembl_version = jnumber("ensembl_version"),
            reference_build = jstring("reference_build")
        )

.variant_types_path <- function(json)
    json %>% 
        spread_values(id = jnumber("id")) %>%
        enter_object("variant_types") %>% gather_array %>%
        spread_values(
            variant_type_id = jnumber("id"),
            name = jstring("name"),
            so_id = jstring("so_id"),
            url = jstring("url"),
            description = jstring("description"),
            display_name = jstring("display_name")
        )

#' @rdname CIViC_API
#' @export
variants <-
    function(page=1, count=25)
{
    stopifnot(is_single_number(page), is_single_number(count))
    response <- .get("variants", c(page=page, count=count))

    json <- content(response, as="text")
    meta <- .meta_path(json %>% enter_object("_meta"))
    records <- json %>% enter_object("records") %>% gather_array

    variant <- records %>%
        spread_values(
            id = jnumber("id"),
            entrez_name = jstring("entrez_name"),
            gene_id = jnumber("gene_id"),
            name = jstring("name"),
            description = jstring("description"),
            type = jstring("type")
        ) %>% enter_object("coordinates") %>%
        .variant_coodinates %>%
        select(-document.id, -array.index)

    variant_types <- .variant_types_path(records) %>%
         select(-document.id, -array.index)

    evidence_items <- records %>%
        spread_values(id = jnumber("id")) %>%
        enter_object("evidence_items") %>%
        spread_values(
            accepted_count = jnumber("accepted_count"),
            rejected_count = jnumber("rejected_count"),
            submitted_count = jnumber("submitted_count")
        ) %>%
        select(id, accepted_count, rejected_count, submitted_count) 
    
    src_CIViC(meta=meta, variant=variant, variant_types = variant_types,
              evidence_items = evidence_items)
}

#' @rdname CIViC_API
#' @export
variant_detail <-
    function(id)
{
    id <- as.character(id)
    stopifnot(is_single_character(id))
    query <- sprintf("variants/%s", id)
    response <- .get(query)

    json <- content(response, as="text")

    variant <- json %>%
        spread_values(
            id = jnumber("id"),
            entrez_name = jstring("entrez_name"),
            gene_id = jnumber("gene_id"),
            entrez_id = jnumber("entrez_id"),
            name = jstring("name"),
            description = jstring("description"),
            type = jstring("type")
        ) %>%
        enter_object("coordinates") %>%
        .variant_coodinates %>%
        select(-document.id)

    variant_types <- .variant_types_path(json) %>% select(-document.id)
    ## evidence_items
    ## variant_groups
    ## variant_aliases
    ## lifecycle_actions
    sources <- .source_path(json)

    src_CIViC(variant=variant, variant_types = variant_types, source = sources)
}

#' @rdname CIViC_API
#' @export
variants_comments <- function(id)
    .comments("variants", id)

## evidence

#' @rdname CIViC_API
#' @export
evidence <-
    function(page=1, count=25)
{}

#' @rdname CIViC_API
#' @export
evidence_detail <-
    function(id)
{}

#' @rdname CIViC_API
#' @export
evidence_comments <- function(id)
    .comments("evidence", id)

#' @rdname CIViC_API
#' @export
evidence_suggested_changes <-
    function(id)
{}

#' @rdname CIViC_API
#' @export
evidence_suggested_changes_comments <-
    function(id, cid)
{}

## variant groups

#' @rdname CIViC_API
#' @export
variant_groups <-
    function(page=1, count=25)
{}

#' @rdname CIViC_API
#' @export
variant_groups_comments <- function(id)
    .comments("variant_groups", id)
