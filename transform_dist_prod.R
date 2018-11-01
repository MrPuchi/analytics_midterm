# Identify the top ditributors
sort(table(imdb.data.noNA.genres$distributor))

# Create a new distributor vector with the Top 12 names
distributor_shortlist = names(head(sort(table(imdb.data.noNA.genres$distributor), decreasing = TRUE), 12))
distributor_shortlist
distributor_1 = ifelse(imdb.data.noNA.genres$distributor %in% distributor_shortlist, levels(imdb.data.noNA.genres$distributor)[imdb.data.noNA.genres$distributor], "Other")
table(distributor_1)

# Identify the top production houses
sort(table(imdb.data.noNA.genres$production_company))

# Create a new distributor vector with the Top 8 names
production_shortlist = names(head(sort(table(imdb.data.noNA.genres$production_company), decreasing = TRUE), 8))
production_shortlist
production_company_1 = ifelse(imdb.data.noNA.genres$production_company %in% production_shortlist, levels(imdb.data.noNA.genres$production_company)[imdb.data.noNA.genres$production_company], "Other")
table(production_company_1)

