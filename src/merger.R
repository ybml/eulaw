# --------------------------------------------------------------------------- #
# Merger Treaty
# --------------------------------------------------------------------------- #

# August 2, 2017
merger = read_html("https://en.wikisource.org/wiki/Merger_Treaty")

treaty = merger %>%
  html_nodes("h2,h3,p,dd") %>%
  html_text()

merger = data.frame(text = treaty)

merger$index = seq_len(nrow(merger))

# table of contents
merger_toc = get_toc("https://en.wikisource.org/wiki/Merger_Treaty")

# chapters and articles
merger_toc = merger_toc %>% 
  separate(tocnumber, into = c("chapter", "article"), sep = "\\.", remove = FALSE)

# delete [edit]
merger$text = str_replace_all(merger$text, "\\[edit\\]", " ")

names(merger_toc)[names(merger_toc)=="toctext"] <- "text"

merger_toc = merger_toc[,c("text", "chapter")]

# make new article variable
merger_toc$article = as.numeric(unlist(str_extract_all(str_extract_all(merger_toc$text, "^Article \\d{1,}"), "\\d{1,}")))
merger_toc$article[merger_toc$article == 0] = NA

# trim whitespace
merger_toc$text = trim(merger_toc$text)
merger$text = trim(merger$text)

# merge
merger = merge(merger, merger_toc, by = "text", all = TRUE)
rm(merger_toc)

# sort 
merger = merger[order(merger$index),]

# fill chapter numbers
merger = merger %>% fill(chapter)
# fill articles
merger = merger %>% fill(article)

# remove chapters 
merger$remove = str_extract_all(merger$text, "^CHAPTER")
merger = subset(merger, merger$remove != "CHAPTER")
merger = subset(merger, select = c("text", "index", "chapter", "article"))

# concatenate text
merger = merger[order(merger$index),]
merger = subset(merger, merger$index >= 26 & merger$index <= 202)

merger = merger %>%
  group_by(chapter, article) %>%
  summarize(text = paste(text, collapse = " "))

merger = merger[order(merger$article),]

write.csv(merger, file = "data/merger.csv")

# create id variable -------------------------------------------------------- #

merger = merger %>% 
  unite(merger_id, chapter:article, sep = ".")

merger$merger_id = str_replace_all(merger$merger_id, "NA", "X")
merger$treaty = "merger"

merger = merger %>% 
  select(treaty, merger_id, text)

# changes ------------------------------------------------------------------- #

to_be_changed <- readRDS("data/1957_2.rds")

# change NA to "N/A"
to_be_changed[is.na(to_be_changed)] = "N/A"

# articles 1, 2, 3, 4, 5, 6 ------------------------------------------------- #
sub_merger = subset(merger, merger$merger_id == "1.1" | merger$merger_id == "1.2" | merger$merger_id == "1.3" |
                            merger$merger_id == "1.4" | merger$merger_id == "1.5" | merger$merger_id == "1.6")

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# article 7 ----------------------------------------------------------------- #
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.3.27", ]
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.3.29", ]
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.3.30", ]

# first paragraph of Article 28: check http://www.ab.gov.tr/files/ardb/evt/1_avrupa_birligi/1_3_antlasmalar/1_3_1_kurucu_antlasmalar/1951_treaty_establishing_ceca.pdf
to_be_changed$text[to_be_changed$ecsc_id == "2.3.28"] = gsub("Meetings of the Council shall be called by its President on the request of a State or of the High Authority. ","",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.3.28"])
  
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.2.146", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.2.147", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.2.151", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.2.154", ]

to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.2.116", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.2.117", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.2.121", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.2.123", ]

# article 8 ----------------------------------------------------------------- #

to_be_changed$text[to_be_changed$ecsc_id == "2.3.28"] = gsub("Wherever the present Treaty requires a unanimous decision or unanimous concurrence, such decision or concurrence will be adopted if supported by the votes of all of the members of the Council.",
                                                             "Wherever the present Treaty requires a unanimous decision or unanimous concurrence, such decision or concurrence will be adopted if supported by the votes of all of the members of the Council. However, for the purposes of applying Articles 21, 32, 32a, 78d and 78f of this Treaty, and Article 16, the third paragraph of Article 20, the fifth paragraph of Article 28 and Article 44 of the Protocol on the Statute of the Court of Justice, abstention by members present in person or represented shall not prevent the adoption by the Council of acts which require unanimity.",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.3.28"])

to_be_changed$text[to_be_changed$ecsc_id == "2.3.28"] = gsub("The decisions of the Council, other than those which require a qualified majority or a unanimous vote, will be taken by a vote of the majority of the total membership. This majority shall be deemed to exist if it includes the absolute majority of the representatives of the member States including the vote of the representative of one of the States which produces at least twenty percent of the total value of coal and steel produced in the Community.",
                                                             "The decisions of the Council, other than those which require a qualified majority or a unanimous vote, will be taken by a vote of the majority of the total membership. This majority shall be deemed to exist if it includes the absolute majority of the representatives of the member States including the vote of the representative of one of the States which produces at least twenty percent of the total value of coal and steel produced in the Community. However, for the purposes of applying those provisions of Articles 78, 78b and 78d of this Treaty which require a qualified majority, the votes of the members of the Council shall be weighted as follows: Belgium 2, Germany 4, France 4, Italy 4, Luxembourg 1, Netherlands 2. For their adoption, acts shall require at least twelve votes in favour, cast by not less than four members.",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.3.28"])

# Protocol on the Statute of the Court of Justice

# article 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 ----------------------------- #

sub_merger = subset(merger, merger$merger_id == "2.9" | merger$merger_id == "2.10" | merger$merger_id == "2.11" |
                            merger$merger_id == "2.12" | merger$merger_id == "2.13" | merger$merger_id == "2.14" |
                            merger$merger_id == "2.15" | merger$merger_id == "2.16" | merger$merger_id == "2.17" |
                            merger$merger_id == "2.18")

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# article 19 ---------------------------------------------------------------- #

# repeal eec
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.156", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.157", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.158", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.159", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.160", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.161", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.162", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "5.1.1.3.163", ]

# repeal euratom
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.125", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.126", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.127", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.128", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.129", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.130", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.131", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.132", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "3.1.3.133", ]

# repeal ecsc
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.1.9", ]
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.1.10", ]
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.1.11", ]
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.1.12", ]
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.1.13", ]

to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "2.1.17", ]

to_be_changed$text[to_be_changed$ecsc_id == "2.1.16"] = gsub("Within the framework of general organizational regulations established by the High Authority, the President of the High Authority shall be responsible for the administration of its services, and shall insure the execution of the acts of the High Authority. ","",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.1.16"])

to_be_changed$text[to_be_changed$ecsc_id == "2.1.18"] = gsub("The allowances of members of the Consultative Committee shall be determined by the Council on proposal by the High Authority. ","",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.1.18"])

# article 20 ---------------------------------------------------------------- #

sub_merger = subset(merger, merger$merger_id == "3.20")

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# article 21 ---------------------------------------------------------------- #

to_be_changed$text[to_be_changed$ecsc_id == "4.X.78"] = "Article 78 l. The financial year of the Community shall extend from l January to 3l December. 2. The administrative expenditure of the Community shall comprise the expenditure of the High Authority, including that relating to the functioning of the Consultative Committee, of the Court, that of the Assembly, and of the Council. 3. Each institution of the Community shall draw up estimates of its administrative expenditure. The High Authority shall consolidate these estimates in a preliminary draft administrative budget. It shall attach thereto an opinion which may contain estimates diverging from those submitted to it. The High Authority shall place the preliminary draft budget before the Council not later than 30 September of the year preceding that in which the budget is to be implemented. The Council shall consult the High Authority and, where appropriate, the other institutions concerned whenever it intends to depart from the preliminary draft budget. 4. The Council shall, acting by a qualified majority, draw up the draft administrative budget, and then transmit it to the Assembly. The draft administrative budget shall be laid before the Assembly not later than 3l October of the year preceding that in which it is to be operative. The Assembly shall be entitled to propose amendments to the draft administrative budget, to the Council. 5. If, within a period of one month from the receipt of the draft administrative budget, the Assembly has given its approval or has not made its opinion known to the Council, the draft administrative budget shall be deemed to be finally adopted. If within this period the Assembly has proposed any amendments, the draft administrative budget so amended shall be transmitted to the Council. The Council shall then discuss it with the High Authority and, where appropriate, with the other institutions concerned, and shall then finally adopt the administrative budget, acting by a qualified majority. 6. The final adoption of the administrative budget shall have the effect of authorising and requiring the High Authority to collect the corresponding revenue in accordance with the provisions of Article 49." 

text = c("Article 78a The administrative budget shall be drawn up in the unit of account determined in accordance with the provisions of the regulations adopted pursuant to Article 78f. The expenditure provided for in the administrative budget shall be authorised for one financial year, unless the regulations adopted pursuant to Article 78f provide otherwise. In accordance with conditions to be laid down pursuant to Article 78f, any appropriations, other than those relating to staff expenditure, that are unspent at the end of the financial year may, but may only, be carried forward to the next financial year. Appropriations shall be classified under different chapters grouping items of expenditure according to their nature or purpose and subdivided, as far as may be necessary, in accordance with the regulations made pursuant to Article 78f. The expenditure of the Assembly, the Council, the High Authority and the Court shall be set out in separate parts of the administrative budget, without prejudice to special arrangements for certain items of common expenses.",
"Article 78b 1. If, at the beginning of the financial year, the administrative budget has not yet been approved, a sum equivalent to not more than one-twelfth of the budget appropriations for the preceding financial year may be spent each month in respect of any chapter or other subdivision of the administrative budget in accordance with the provisions of the regulations adopted pursuant to Article 78f; this arrangement shall not, however, have the effect of placing at the disposal of the High Authority appropriations in excess of one-twelfth of those provided for in the draft administrative budget in course of preparation. The High Authority is authorised and required to impose the levies up to the amount of the appropriations for the preceding financial year, but shall not thereby exceed the amount which would result from the adoption of the draft administrative budget. 2. The Council may, acting by a qualified majority, provided that the other conditions laid down in the first paragraph of this Article are observed, authorise expenditure in excess of one-twelfth of the appropriations. The authorisation and requirement to collect levies may be adapted accordingly.", 
"Article 78c The High Authority shall, in accordance with the provisions of the regulations adopted pursuant to Article 78f, implement the administrative budget on its own responsibility and within the limits of the appropriations allotted. The regulations shall lay down, in detail, exactly how each institution is to expend the funds allocated to it. Within the administrative budget, the High Authority may, subject to the limits and conditions laid down in the regulations adopted pursuant to Article 78f, transfer appropriations from one chapter to another or from one subdivision to another.", 
"Article 78d The accounts of all the administrative expenditure referred to in Article 78(2), and of administrative revenue and of revenue derived from the tax for the benefit of the Community levied on the salaries, wages and emoluments of its officials and other servants, shall be examined by an Audit Board consisting of auditors whose independence is beyond doubt, one of whom shall be chairman. The Council shall, acting unanimously, determine the number of the auditors. The auditors and the chairman of the Audit Board shall be appointed by the Council, acting unanimously, for a period of five years. Their remuneration shall be determined by the Council, acting by a qualified majority. The purpose of the audit, which shall be based on records and, if necessary, performed on the spot, shall be to establish that all revenue has been received and all expenditure incurred in a lawful and regular manner and that the financial management has been sound. After the close of each financial year, the Audit Board shall draw up a report, which shall be adopted by a majority of its members. The High Authority shall submit annually to the Council and to the Assembly the accounts of the preceding financial year relating to the implementation of the administrative budget, together with the report of the Audit Board. The High Authority shall also forward to them a financial statement of the assets and liabilities of the Community in the field covered by that budget. The Council shall, acting by a qualified majority, give a discharge to the High Authority in respect of the implementation of the administrative budget. It shall communicate its decision to the Assembly.", 
"Article 78e The Council shall appoint an auditor to serve for three years; he shall draw up an annual report stating whether the accounting and the financial management of the High Authority have been effected in a regular manner; this report shall not cover entries relating to the administrative expenditure referred to in Article 78(2), to administrative revenue or to revenue derived from the tax for the benefit of the Community levied on the salaries, wages and emoluments of its officials and other servants. He shall draw up this report within six months of the close of the financial year to which the accounts refer and shall submit it to the High Authority and the Council. The High Authority shall forward it to the Assembly. The auditor shall be completely independent in the performance of his duties. The office of auditor shall be incompatible with any other office in an institution or department of the Communities other than that of member of the Audit Board provided for in Article 78d. His term of office shall be renewable.", 
"Article 78f The Council shall, acting unanimously on a proposal from the High Authority: (a) make financial regulations specifying in particular the procedure to be adopted for establishing and implementing the administrative budget and for presenting and auditing accounts; (b) lay down rules concerning the responsibility of authorising officers and accounting officers and concerning appropriate arrangements for inspection.")

ecsc_id = c("4.X.78a", "4.X.78b", "4.X.78c", "4.X.78d", "4.X.78e", "4.X.78f")
treaty = c("merger", "merger", "merger", "merger", "merger", "merger")

sub_merger = data.frame(text, ecsc_id, treaty)
rm(text, ecsc_id, treaty)

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"
  
# article 22 ---------------------------------------------------------------- #

sub_merger = subset(merger, merger$merger_id == "3.22")

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# article 23 ---------------------------------------------------------------- #

# Convention on Certain Institutions Common to the European Communities

# article 24 ---------------------------------------------------------------- #

sub_merger = subset(merger, merger$merger_id == "4.24")

sub_merger$text = "Article 24 1. The officials and other servants of the European Coal and Steel Community, the European Economic Community and the European Atomic Energy Community shall, at the date of entry into force of this Treaty, become officials and other servants of the European Communities and form part of the single administration of those Communities. The Council shall, acting by a qualified majority on a proposal from the Commission and after consulting the other institutions concerned, lay down the staff regulations of officials of the European Communities and the conditions of employment of other servants of those Communities."

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# Convention on the Transitional Provisions annexed to the Treaty establishing the European Coal and Steel Community, 

to_be_changed = to_be_changed[!to_be_changed$eec_id == "6.X.X.X.212", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "5.X.X.186", ]

# article 25 ---------------------------------------------------------------- #

sub_merger = subset(merger, merger$merger_id == "4.25")

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# article 26 ---------------------------------------------------------------- #

to_be_changed$text[to_be_changed$ecsc_id == "2.4.40"] = gsub("It shall also have jurisdiction to assess damages against any official or employee of the Community, in cases where injury results from a personal fault of such official or employee in the performance of his duties. If the injured party is unable to recover such damages from such official or employee, the Court may assess an equitable indemnity against the Community.",
                                                             "The Court shall also have jurisdiction to order the Community to make good any injury caused by a personal wrong by a servant of the Community in the performance of his duties. The personal liability of its servants towards the Community shall be governed by the provisions laid down in their Staff Regulations or the Conditions of Employment applicable to them.",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.4.40"])

# article 27 ---------------------------------------------------------------- #

to_be_changed$text[to_be_changed$ecsc_id == "2.2.22"] = gsub("The Assembly shall hold an annual session.",
                                                             "The Assembly shall hold an annual session. It shall meet, without requiring to be convened, on the second Tuesday in March.",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.2.22"])

to_be_changed$text[to_be_changed$eec_id == "5.1.1.1.139"] = gsub("The Assembly shall hold an annual session.",
                                                             "The Assembly shall hold an annual session. It shall meet, without requiring to be convened, on the second Tuesday in March.",
                                                             to_be_changed$text[to_be_changed$eec_id == "5.1.1.1.139"])

to_be_changed$text[to_be_changed$euratom_id == "3.1.1.109"] = gsub("The Assembly shall hold an annual session.",
                                                             "The Assembly shall hold an annual session. It shall meet, without requiring to be convened, on the second Tuesday in March.",
                                                             to_be_changed$text[to_be_changed$euratom_id == "3.1.1.109"])

to_be_changed$text[to_be_changed$ecsc_id == "2.2.24"] = gsub("If a motion of censure on the report is presented to the Assembly, a vote may be taken thereon only after a period of not less than three days following its introduction, and such vote shall be by open ballot.",
                                                             "If a motion of censure on the activities of the High Authority is tabled before it, the Assembly shall not vote thereon until at least three days after the motion has been tabled and only by open vote",
                                                             to_be_changed$text[to_be_changed$ecsc_id == "2.2.24"])

# article 28 ---------------------------------------------------------------- #

sub_merger = subset(merger, merger$merger_id == "5.28")

sub_merger$text = "Article 28 The European Communities shall enjoy in the territories of the Member States such privileges and immunities as are necessary for the performance of their tasks, under the conditions laid down in the Protocol annexed to this Treaty. The same shall apply to the European Investment Bank."

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# repeal
to_be_changed = to_be_changed[!to_be_changed$ecsc_id == "4.X.76", ]
to_be_changed = to_be_changed[!to_be_changed$eec_id == "6.X.X.X.218", ]
to_be_changed = to_be_changed[!to_be_changed$euratom_id == "5.X.X.191", ]

# Protocols on Privileges and Immunities 
# Protocol on the Statute of the Court of Justice annexed to the Treaty establishing the European Coal and Steel Community
# Protocol on the Statute of the European Investment Bank annexed to the Treaty establishing the European Economic Community

# articles 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 29 ----------------------- #

sub_merger = subset(merger, merger$merger_id == "5.29" | merger$merger_id == "5.30" | merger$merger_id == "5.31" |
                            merger$merger_id == "5.32" | merger$merger_id == "5.33" | merger$merger_id == "5.34" |
                            merger$merger_id == "5.35" | merger$merger_id == "5.36" | merger$merger_id == "5.37" |
                            merger$merger_id == "5.38" | merger$merger_id == "5.39")

to_be_changed = bind_rows(to_be_changed, sub_merger)
to_be_changed[is.na(to_be_changed)] = "N/A"

# save ---------------------------------------------------------------------- #
saveRDS(to_be_changed, "data/1965.rds")

rm(merger, sub_merger, to_be_changed)



