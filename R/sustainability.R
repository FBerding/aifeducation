# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Summarizing tracked sustainability data
#' @description Function for summarizing the tracked sustainability data with a tracker of the python library
#'   'codecarbon'.
#'
#' @param sustainability_tracker Object of class `codecarbon.emissions_tracker.OfflineEmissionsTracker` of the python
#'   library codecarbon.
#' @return Returns a `list` which contains the tracked sustainability data.
#'
#' @family Auxiliary Functions
#'
#' @keywords internal
summarize_tracked_sustainability <- function(sustainability_tracker) {
  if (is.null(sustainability_tracker$final_emissions_data$region)) {
    region <- NA
  } else {
    region <- sustainability_tracker$final_emissions_data$region
  }

  if (is.null(sustainability_tracker$final_emissions_data$gpu_count)) {
    gpu_count_tmp <- NA
  } else {
    gpu_count_tmp <- sustainability_tracker$final_emissions_data$gpu_count
  }

  if (is.null(sustainability_tracker$final_emissions_data$gpu_model)) {
    gpu_model_tmp <- NA
  } else {
    gpu_model_tmp <- sustainability_tracker$final_emissions_data$gpu_model
  }

  results <- list(
    sustainability_tracked = TRUE,
    date = date(),
    sustainability_data = list(
      duration_sec = sustainability_tracker$final_emissions_data$duration,
      co2eq_kg = sustainability_tracker$final_emissions_data$emissions,
      cpu_energy_kwh = sustainability_tracker$final_emissions_data$cpu_energy,
      gpu_energy_kwh = sustainability_tracker$final_emissions_data$gpu_energy,
      ram_energy_kwh = sustainability_tracker$final_emissions_data$ram_energy,
      total_energy_kwh = sustainability_tracker$final_emissions_data$energy_consumed
    ),
    technical = list(
      tracker = "codecarbon",
      py_package_version = codecarbon$"__version__",
      cpu_count = sustainability_tracker$final_emissions_data$cpu_count,
      cpu_model = sustainability_tracker$final_emissions_data$cpu_model,
      gpu_count = gpu_count_tmp,
      gpu_model = gpu_model_tmp,
      ram_total_size = sustainability_tracker$final_emissions_data$ram_total_size
    ),
    region = list(
      country_name = sustainability_tracker$final_emissions_data$country_name,
      country_iso_code = sustainability_tracker$final_emissions_data$country_iso_code,
      region = region
    )
  )
  return(results)
}


#' @title Country Alpha 3 Codes
#'
#' @description Function for requesting a `vector` containing the alpha-3 codes for most countries.
#' @return Returns a `vector` containing the alpha-3 codes for most countries.
#' @family Auxiliary Functions
#'
#' @export
get_alpha_3_codes <- function() {
  codes <- list(
    Afghanistan = "AFG",
    Aland_Islands = "ALA",
    Albania = "ALB",
    Algeria = "DZA",
    American_Samoa = "ASM",
    Andorra = "AND",
    Angola = "AGO",
    Anguilla = "AIA",
    Antarctica = "ATA",
    Antigua_and_Barbuda = "ATG",
    Argentina = "ARG",
    Armenia = "ARM",
    Aruba = "ABW",
    Australia = "AUS",
    Austria = "AUT",
    Azerbaijan = "AZE",
    Bahamas = "BHS",
    Bahrain = "BHR",
    Bangladesh = "BGD",
    Barbados = "BRB",
    Belarus = "BLR",
    Belgium = "BEL",
    Belize = "BLZ",
    Benin = "BEN",
    Bermuda = "BMU",
    Bhutan = "BTN",
    Bolivia_Plurinational_State_of = "BOL",
    Bonaire_Sint_Eustatius_and_Saba = "BES",
    Bosnia_and_Herzegovina = "BIH",
    Botswana = "BWA",
    Bouvet_Island = "BVT",
    Brazil = "BRA",
    British_Indian_Ocean_Territory = "IOT",
    Brunei_Darussalam = "BRN",
    Bulgaria = "BGR",
    Burkina_Faso = "BFA",
    Burundi = "BDI",
    Cabo_Verde = "CPV",
    Cambodia = "KHM",
    Cameroon = "CMR",
    Canada = "CAN",
    Cayman_Islands = "CYM",
    Central_African_Republic = "CAF",
    Chad = "TCD",
    Chile = "CHL",
    China = "CHN",
    Christmas_Island = "CXR",
    Cocos_Keeling_Islands = "CCK",
    Colombia = "COL",
    Comoros = "COM",
    Congo = "COG",
    Congo_Democratic_Republic_of_the = "COD",
    Cook_Islands = "COK",
    Costa_Rica = "CRI",
    Cote_d_Ivoire = "CIV",
    Croatia = "HRV",
    Cuba = "CUB",
    Curacao = "CUW",
    Cyprus = "CYP",
    Czechia = "CZE",
    Denmark = "DNK",
    Djibouti = "DJI",
    Dominica = "DMA",
    Dominican_Republic = "DOM",
    Ecuador = "ECU",
    Egypt = "EGY",
    El_Salvador = "SLV",
    Equatorial_Guinea = "GNQ",
    Eritrea = "ERI",
    Estonia = "EST",
    Eswatini = "SWZ",
    Ethiopia = "ETH",
    Falkland_Islands_Malvinas = "FLK",
    Faroe_Islands = "FRO",
    Fiji = "FJI",
    Finland = "FIN",
    France = "FRA",
    French_Guiana = "GUF",
    French_Polynesia = "PYF",
    French_Southern_Territories = "ATF",
    Gabon = "GAB",
    Gambia = "GMB",
    Georgia = "GEO",
    Germany = "DEU",
    Ghana = "GHA",
    Gibraltar = "GIB",
    Greece = "GRC",
    Greenland = "GRL",
    Grenada = "GRD",
    Guadeloupe = "GLP",
    Guam = "GUM",
    Guatemala = "GTM",
    Guernsey = "GGY",
    Guinea = "GIN",
    Guinea_Bissau = "GNB",
    Guyana = "GUY",
    Haiti = "HTI",
    Heard_Island_and_McDonald_Islands = "HMD",
    Holy_See = "VAT",
    Honduras = "HND",
    Hong_Kong = "HKG",
    Hungary = "HUN",
    Iceland = "ISL",
    India = "IND",
    Indonesia = "IDN",
    Iran_Islamic_Republic_of = "IRN",
    Iraq = "IRQ",
    Ireland = "IRL",
    Isle_of_Man = "IMN",
    Israel = "ISR",
    Italy = "ITA",
    Jamaica = "JAM",
    Japan = "JPN",
    Jersey = "JEY",
    Jordan = "JOR",
    Kazakhstan = "KAZ",
    Kenya = "KEN",
    Kiribati = "KIR",
    Korea_Democratic_Peoples_Republic_of = "PRK",
    Korea_Republic_of = "KOR",
    Kuwait = "KWT",
    Kyrgyzstan = "KGZ",
    Lao_Peoples_Democratic_Republic = "LAO",
    Latvia = "LVA",
    Lebanon = "LBN",
    Lesotho = "LSO",
    Liberia = "LBR",
    Libya = "LBY",
    Liechtenstein = "LIE",
    Lithuania = "LTU",
    Luxembourg = "LUX",
    Macao = "MAC",
    Madagascar = "MDG",
    Malawi = "MWI",
    Malaysia = "MYS",
    Maldives = "MDV",
    Mali = "MLI",
    Malta = "MLT",
    Marshall_Islands = "MHL",
    Martinique = "MTQ",
    Mauritania = "MRT",
    Mauritius = "MUS",
    Mayotte = "MYT",
    Mexico = "MEX",
    Micronesia_Federated_States_of = "FSM",
    Moldova_Republic_of = "MDA",
    Monaco = "MCO",
    Mongolia = "MNG",
    Montenegro = "MNE",
    Montserrat = "MSR",
    Morocco = "MAR",
    Mozambique = "MOZ",
    Myanmar = "MMR",
    Namibia = "NAM",
    Nauru = "NRU",
    Nepal = "NPL",
    Netherlands = "NLD",
    New_Caledonia = "NCL",
    New_Zealand = "NZL",
    Nicaragua = "NIC",
    Niger = "NER",
    Nigeria = "NGA",
    Niue = "NIU",
    Norfolk_Island = "NFK",
    North_Macedonia = "MKD",
    Northern_Mariana_Islands = "MNP",
    Norway = "NOR",
    Oman = "OMN",
    Pakistan = "PAK",
    Palau = "PLW",
    Palestine_State_of = "PSE",
    Panama = "PAN",
    Papua_New_Guinea = "PNG",
    Paraguay = "PRY",
    Peru = "PER",
    Philippines = "PHL",
    Pitcairn = "PCN",
    Poland = "POL",
    Portugal = "PRT",
    Puerto_Rico = "PRI",
    Qatar = "QAT",
    Reunion = "REU",
    Romania = "ROU",
    Russian_Federation = "RUS",
    Rwanda = "RWA",
    Saint_Barthelemy = "BLM",
    Saint_Helena_Ascension_and_Tristan_da_Cunha = "SHN",
    Saint_Kitts_and_Nevis = "KNA",
    Saint_Lucia = "LCA",
    Saint_Martin_French_part = "MAF",
    Saint_Pierre_and_Miquelon = "SPM",
    Saint_Vincent_and_the_Grenadines = "VCT",
    Samoa = "WSM",
    San_Marino = "SMR",
    Sao_Tome_and_Principe = "STP",
    Saudi_Arabia = "SAU",
    Senegal = "SEN",
    Serbia = "SRB",
    Seychelles = "SYC",
    Sierra_Leone = "SLE",
    Singapore = "SGP",
    Sint_Maarten_Dutch_part = "SXM",
    Slovakia = "SVK",
    Slovenia = "SVN",
    Solomon_Islands = "SLB",
    Somalia = "SOM",
    South_Africa = "ZAF",
    South_Georgia_and_the_South_Sandwich_Islands = "SGS",
    South_Sudan = "SSD",
    Spain = "ESP",
    Sri_Lanka = "LKA",
    Sudan = "SDN",
    Suriname = "SUR",
    Svalbard_and_Jan_Mayen = "SJM",
    Sweden = "SWE",
    Switzerland = "CHE",
    Syrian_Arab_Republic = "SYR",
    Taiwan_Province_of_China = "TWN",
    Tajikistan = "TJK",
    Tanzania_United_Republic_of = "TZA",
    Thailand = "THA",
    Timor_Leste = "TLS",
    Togo = "TGO",
    Tokelau = "TKL",
    Tonga = "TON",
    Trinidad_and_Tobago = "TTO",
    Tunisia = "TUN",
    Turkey = "TUR",
    Turkmenistan = "TKM",
    Turks_and_Caicos_Islands = "TCA",
    Tuvalu = "TUV",
    Uganda = "UGA",
    Ukraine = "UKR",
    United_Arab_Emirates = "ARE",
    United_Kingdom_of_Great_Britain_and_Northern_Ireland = "GBR",
    United_States_of_America = "USA",
    United_States_Minor_Outlying_Islands = "UMI",
    Uruguay = "URY",
    Uzbekistan = "UZB",
    Vanuatu = "VUT",
    Venezuela_Bolivarian_Republic_of = "VEN",
    Viet_Nam = "VNM",
    Virgin_Islands_British = "VGB",
    Virgin_Islands_U_S_ = "VIR",
    Wallis_and_Futuna = "WLF",
    Western_Sahara = "ESH",
    Yemen = "YEM",
    Zambia = "ZMB",
    Zimbabwe = "ZWE"
  )
  codes <- unlist(codes)
  return(codes)
}
