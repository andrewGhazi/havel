
parula = c("#352A87", "#342D8F", "#343197", "#34359F", "#3439A7", "#343DAF",
           "#2F41B7", "#2946BE", "#234BC6", "#1D50CE", "#1755D5", "#125AD9",
           "#0F5EDB", "#0C61DC", "#0865DD", "#0569DF", "#036CDF", "#066FDE",
           "#0872DC", "#0B75DB", "#0D77DA", "#107AD8", "#107CD7", "#117FD6",
           "#1181D5", "#1284D4", "#1286D3", "#1189D2", "#0F8CD2", "#0D90D1",
           "#0B93D1", "#0996D1", "#0898CF", "#079BCE", "#079DCC", "#06A0CB",
           "#06A2C9", "#06A4C7", "#07A6C4", "#08A8C2", "#0AAABF", "#0BABBC",
           "#0DADB9", "#11AEB6", "#16B0B2", "#1AB1AF", "#1FB2AC", "#23B4A8",
           "#29B5A5", "#2FB6A1", "#36B89E", "#3CB99A", "#42BA97", "#49BB93",
           "#51BC8F", "#58BC8B", "#60BD88", "#67BD84", "#6EBE80", "#75BE7D",
           "#7DBE7A", "#84BE77", "#8BBE74", "#91BE71", "#97BE6F", "#9DBE6C",
           "#A3BD6A", "#A9BD67", "#AFBC65", "#B5BC63", "#BABC61", "#C0BB5F",
           "#C5BB5C", "#CBBA5A", "#D0BA58", "#D5BA56", "#DAB953", "#E0B951",
           "#E5B94F", "#E9B94B", "#EEBA48", "#F2BB44", "#F6BC40", "#FBBD3D",
           "#FCC03A", "#FCC336", "#FBC633", "#FBC930", "#FBCD2D", "#FAD02A",
           "#F9D428", "#F8D725", "#F7DB22", "#F5DF20", "#F5E31D", "#F5E71A",
           "#F6EC17", "#F7F114", "#F8F611", "#F9FB0E")

pull1 = function(l, x, i = 1) {
  l[[x]][[i]]
}

clean_pkg_nm = function(pkg, pak_res) {

  # This should handle pkg = "." I think?
  if (pkg != sbt(pak_res, direct)$ref[1]) pkg = sbt(pak_res, direct)$package[1]

  # TODO: strip repo owner names from pkg
  if (grepl("\\/", pkg)) pkg = pak_res |> sbt(direct) |> getElement('package')

  pkg
}
