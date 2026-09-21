niveles <- c("NACIONAL",
  "A/B", "C+", "C", "C-", "D+", "D", "E",
  "Generación Z (18-28)", "Millennials (29-42)", "Generación X (43-58)", "Baby Boomers (59-67)", "Silenciosos (67 o +)",
  "Hombre", "Mujer",
  "Norte", "Occidente", "Centro", "Sureste",
  "Universidad o más", "Bachillerato o símil", "Secundaria", "Primaria", "Sin estudios",            
  "MOVIMIENTO CIUDADANO", "MORENA-PT-PVEM", "PAN-PRI-PRD", "Indeciso", "Anulará su voto",
  ""
)

shinyLink <- function(to, label) {
  tags$a(
    class="shiny__link",
    href = to,
    label
  )
}

#Línea de tiempo----
graf_pos_neg_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Negativa", data = df %>% filter(opinion %in% c("Negativa", "Negativo", "Desaprueba", "Camino equivocado")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Positiva", data = df %>% filter(opinion %in% c("Positiva", "Positivo", "Aprueba", "Buen camino")) %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_seguridad_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Inseguro", data = df %>% filter(opinion %in% c("Inseguro")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Seguro", data = df %>% filter(opinion %in% c("Seguro")) %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_favorcontra_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "En contra", data = df %>% filter(opinion %in% c("En contra")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "A favor", data = df %>% filter(opinion %in% c("A favor")) %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_tasa_lt <- function(df, titulo) {

  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = unique(df$opinion), data = df %>% pull(por), color = unique(df$Color)) |> 
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = -0.1, max = 1, labels = list(formatter = JS("function() { return (this.value*100).toFixed(0) + '%'; }")),
      plotLines = list(
        list(
          color = "#FF0000",  # Color rojo
          width = 2,          # Ancho de la línea
          value = 0
        )
      )
    ) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_partidos_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "MOVIMIENTO CIUDADANO", data = df %>% filter(opinion == "MOVIMIENTO CIUDADANO") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "MORENA", data = df %>% filter(opinion == "MORENA") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "PAN", data = df %>% filter(opinion == "PAN") %>% pull(por), color = "#0062ff") |>
    hc_add_series(name = "PRI", data = df %>% filter(opinion == "PRI") %>% pull(por), color = "#ff1d25") |>
    hc_add_series(name = "PARTIDO VERDE", data = df %>% filter(opinion == "PARTIDO VERDE") %>% pull(por), color = "#51ce00") |>
    hc_add_series(name = "PRD", data = df %>% filter(opinion == "PRD") %>% pull(por), color = "#ffd400") |>
    hc_add_series(name = "PT", data = df %>% filter(opinion == "PT") %>% pull(por), color = "#a50000") |>
    hc_add_series(name = "Apartidista", data = df %>% filter(opinion == "Apartidista") %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_careos_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", data = df %>% filter(opinion == "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "Claudia Sheinbaum + MORENA-PT-VERDE", data = df %>% filter(opinion == "Claudia Sheinbaum + MORENA-PT-VERDE") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "Xóchitl Gálvez + PAN-PRI-PRD", data = df %>% filter(opinion == "Xóchitl Gálvez + PAN-PRI-PRD") %>% pull(por), color = "#6b9ff1") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(df$medicion)) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_careos_cand_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Jorge Máynez", data = df %>% filter(opinion == "Jorge Máynez") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "Claudia Sheinbaum", data = df %>% filter(opinion == "Claudia Sheinbaum") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "Xóchitl Gálvez", data = df %>% filter(opinion == "Xóchitl Gálvez") %>% pull(por), color = "#6b9ff1") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(df$medicion)) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_alianza_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "MOVIMIENTO CIUDADANO", data = df %>% filter(opinion == "MOVIMIENTO CIUDADANO") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "MORENA-PVEM-PT", data = df %>% filter(opinion == "MORENA-PT-PVEM") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "PAN-PRI-PRD", data = df %>% filter(opinion == "PAN-PRI-PRD") %>% pull(por), color = "#0062ff") |>
    hc_add_series(name = "Anulará su voto", data = df %>% filter(opinion == "Anulará su voto") %>% pull(por), color = "#cccccc") |>
    hc_add_series(name = "Indeciso", data = df %>% filter(opinion == "Indeciso") %>% pull(por), color = "#333333") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_cuatro_partidos_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Que gane MOVIMIENTO CIUDADANO", data = df %>% filter(opinion %in% c("Que gane MOVIMIENTO CIUDADANO")) %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "Que gane MORENA", data = df %>% filter(opinion %in% c("Que gane MORENA")) %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "Que gane el PAN", data = df %>% filter(opinion %in% c("Que gane el PAN")) %>% pull(por), color = "#0062ff") |>
    hc_add_series(name = "Que gane el PRI", data = df %>% filter(opinion %in% c("Que gane el PRI")) %>% pull(por), color = "#ff1d25") |>
    hc_add_series(name = "Que gane  el PAN-PRI-PRD", data = df %>% filter(opinion %in% c("Que gane el PAN-PRI-PRD")) %>% pull(por), color = "#6b9ff1") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion %in% c("Ns/Nc")) %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_sino_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "No", data = df %>% filter(opinion == "No") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Sí", data = df %>% filter(opinion == "Sí") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_economia_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Mejorado", data = df %>% filter(opinion %in% c("Mejorado")) %>% pull(por), color = "#7FDB85") |>
    hc_add_series(name = "Empeorado", data = df %>% filter(opinion %in% c("Empeorado")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Igual que antes", data = df %>% filter(opinion %in% c("Igual que antes", "Igual que antes (espontáneo)")) %>% pull(por), color = "#555555") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_camino_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Camino equivocado", data = df %>% filter(opinion %in% c("Negativa", "Negativo", "Desaprueba", "Camino equivocado")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Camino correcto", data = df %>% filter(opinion %in% c("Positiva", "Positivo", "Aprueba", "Buen camino", "Camino correcto")) %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$medicion))) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}
graf_apoyocandy_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Le da igual (espontáneo)", data = df %>% filter(opinion == "Le da igual (espontáneo)") %>% pull(por), color = "#555555") |>
    hc_add_series(name = "No apoyaría", data = df %>% filter(opinion == "No apoyaría") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Apoyaría", data = df %>% filter(opinion == "Apoyaría") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(df$medicion)) |> 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      line = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_seguvoto_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "gray60") |>
    hc_add_series(name = "1", data = df %>% filter(opinion == "1") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "2", data = df %>% filter(opinion == "2") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "3", data = df %>% filter(opinion == "3") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "4", data = df %>% filter(opinion == "4") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "5", data = df %>% filter(opinion == "5") %>% pull(por), color = "orange") |>
    hc_add_series(name = "6", data = df %>% filter(opinion == "6") %>% pull(por), color = "orange") |>
    hc_add_series(name = "7", data = df %>% filter(opinion == "7") %>% pull(por), color = "orange") |>
    hc_add_series(name = "8", data = df %>% filter(opinion == "8") %>% pull(por), color = "#7FDB85") |>
    hc_add_series(name = "9", data = df %>% filter(opinion == "9") %>% pull(por), color = "#7FDB85") |>
    hc_add_series(name = "10", data = df %>% filter(opinion == "10") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = as.factor(unique(df$medicion))) %>% 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      column = list(
        stacking = "normal",
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_seguvoto_alt_lt <- function(df, titulo) {
  
  highchart() %>%
    hc_chart(type = "area") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "gray60") |>
    hc_add_series(name = "Poco seguro", data = df %>% filter(opinion == "Poco seguro") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Algo seguro", data = df %>% filter(opinion == "Algo seguro") %>% pull(por), color = "orange") |>
    hc_add_series(name = "Muy seguro", data = df %>% filter(opinion == "Muy seguro") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = as.factor(unique(df$medicion))) %>% 
    hc_yAxis(min = 0, labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      area = list(
        stacking = "normal",
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

# Puntuales ----
graf_pos_neg <- function(df, titulo, subtitulo) {
  
  if (length(df$var) == 1) {
    categories <- list("NACIONAL")
  } else {
    categories <- unique(df$var)
  }
  
  options(htmlwidgets.TOJSON_ARGS = list(auto_unbox = TRUE))
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_xAxis(categories = categories) %>%
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Negativa", data = df %>% filter(opinion %in% c("Negativa", "Negativo", "Desaprueba","Camino equivocado")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Positiva", data = df %>% filter(opinion %in% c("Positiva", "Positivo", "Aprueba", "Buen camino")) %>% pull(por), color = "#7FDB85") |>
    hc_plotOptions(series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )) %>%
    hc_tooltip(formatter = JS(
      "function() {
        return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%';
      }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}
  
graf_tasa <- function(df, titulo, subtitulo) {
  
  highchart() |> 
    hc_chart(type = "bar", color = unique(df$Color)) |> 
    hc_xAxis(categories = unique(df$var)) |> 
    hc_yAxis(max= 1, labels = list(formatter = JS("function() { return (this.value*100).toFixed(0) + '%'; }"))) |> 
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = unique(df$filtro), data = df |>  pull(por), color = unique(df$Color)) |> 
    hc_plotOptions(series = list(stacking = "normal"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )) |>
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |>
    hc_legend(enabled = TRUE) |>
    hc_exporting(enabled = TRUE)
}

graf_partidos <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "MOVIMIENTO CIUDADANO", data = df %>% filter(opinion == "MOVIMIENTO CIUDADANO") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "MORENA", data = df %>% filter(opinion == "MORENA") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "PAN", data = df %>% filter(opinion == "PAN") %>% pull(por), color = "#0062ff") |>
    hc_add_series(name = "PRI", data = df %>% filter(opinion == "PRI") %>% pull(por), color = "#ff1d25") |>
    hc_add_series(name = "PARTIDO VERDE", data = df %>% filter(opinion == "PARTIDO VERDE") %>% pull(por), color = "#51ce00") |>
    hc_add_series(name = "PRD", data = df %>% filter(opinion == "PRD") %>% pull(por), color = "#ffd400") |>
    hc_add_series(name = "PT", data = df %>% filter(opinion == "PT") %>% pull(por), color = "#a50000") |>
    hc_add_series(name = "Apartidista", data = df %>% filter(opinion == "Apartidista") %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_careos <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO", data = df %>% filter(opinion == "Jorge Álvarez Máynez + MOVIMIENTO CIUDADANO") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "Claudia Sheinbaum + MORENA-PT-VERDE", data = df %>% filter(opinion == "Claudia Sheinbaum + MORENA-PT-VERDE") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "Xóchitl Gálvez + PAN-PRI-PRD", data = df %>% filter(opinion == "Xóchitl Gálvez + PAN-PRI-PRD") %>% pull(por), color = "#6b9ff1") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_careos_cand <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Jorge Máynez", data = df %>% filter(opinion == "Jorge Máynez") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "Claudia Sheinbaum", data = df %>% filter(opinion == "Claudia Sheinbaum") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "Xóchitl Gálvez", data = df %>% filter(opinion == "Xóchitl Gálvez") %>% pull(por), color = "#6b9ff1") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_sino <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "No", data = df %>% filter(opinion == "No") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Sí", data = df %>% filter(opinion == "Sí") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_alianza <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "MOVIMIENTO CIUDADANO", data = df %>% filter(opinion == "MOVIMIENTO CIUDADANO") %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "MORENA-PVEM-PT", data = df %>% filter(opinion == "MORENA-PT-PVEM") %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "PAN-PRI-PRD", data = df %>% filter(opinion == "PAN-PRI-PRD") %>% pull(por), color = "#0062ff") |>
    hc_add_series(name = "Anulará su voto", data = df %>% filter(opinion == "Anulará su voto") %>% pull(por), color = "#cccccc") |>
    hc_add_series(name = "Indeciso", data = df %>% filter(opinion == "Indeciso") %>% pull(por), color = "#333333") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_cuatro_partidos <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Que gane MOVIMIENTO CIUDADANO", data = df %>% filter(opinion %in% c("Que gane MOVIMIENTO CIUDADANO")) %>% pull(por), color = "#f77b09") |>
    hc_add_series(name = "Que gane MORENA", data = df %>% filter(opinion %in% c("Que gane MORENA")) %>% pull(por), color = "#761d01") |>
    hc_add_series(name = "Que gane el PAN", data = df %>% filter(opinion %in% c("Que gane el PAN")) %>% pull(por), color = "#0062ff") |>
    hc_add_series(name = "Que gane el PRI", data = df %>% filter(opinion %in% c("Que gane el PRI")) %>% pull(por), color = "#ff1d25") |>
    hc_add_series(name = "Que gane  el PAN-PRI-PRD", data = df %>% filter(opinion %in% c("Que gane el PAN-PRI-PRD")) %>% pull(por), color = "#6b9ff1") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion %in% c("Ns/Nc")) %>% pull(por), color = "#cccccc") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_favorcontra <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "En contra", data = df %>% filter(opinion %in% c("En contra")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "A favor", data = df %>% filter(opinion %in% c("A favor")) %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_seguridad <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Inseguro", data = df %>% filter(opinion %in% c("Inseguro")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Seguro", data = df %>% filter(opinion %in% c("Seguro")) %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_economia <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_xAxis(categories = unique(as.character(df$var))) %>%
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value).toFixed(0) + '%'; }")), reversedStacks = TRUE) |> 
    hc_title(text = titulo, style = list(fontWeight = "bold")) |>
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Mejorado", data = df %>% filter(opinion %in% c("Mejorado")) %>% pull(por), color = "#7FDB85") |>
    hc_add_series(name = "Empeorado", data = df %>% filter(opinion %in% c("Empeorado")) %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Igual que antes", data = df %>% filter(opinion %in% c("Igual que antes", "Igual que antes (espontáneo)")) %>% pull(por), color = "#555555") |>
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_plotOptions(series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )) %>%
    hc_tooltip(formatter = JS(
      "function() {
        return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%';
      }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_apoya <- function(df, titulo, subtitulo) {
  
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "#CACACA") |>
    hc_add_series(name = "Le da igual (espontáneo)", data = df %>% filter(opinion == "Le da igual (espontáneo)") %>% pull(por), color = "#555555") |>
    hc_add_series(name = "No apoyaría", data = df %>% filter(opinion == "No apoyaría") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Apoyaría", data = df %>% filter(opinion == "Apoyaría") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}

graf_seguvoto <- function(df, titulo, subtitulo) {
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
  hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "gray60") |>
    hc_add_series(name = "1", data = df %>% filter(opinion == "1") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "2", data = df %>% filter(opinion == "2") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "3", data = df %>% filter(opinion == "3") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "4", data = df %>% filter(opinion == "4") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "5", data = df %>% filter(opinion == "5") %>% pull(por), color = "orange") |>
    hc_add_series(name = "6", data = df %>% filter(opinion == "6") %>% pull(por), color = "orange") |>
    hc_add_series(name = "7", data = df %>% filter(opinion == "7") %>% pull(por), color = "orange") |>
    hc_add_series(name = "8", data = df %>% filter(opinion == "8") %>% pull(por), color = "#7FDB85") |>
    hc_add_series(name = "9", data = df %>% filter(opinion == "9") %>% pull(por), color = "#7FDB85") |>
    hc_add_series(name = "10", data = df %>% filter(opinion == "10") %>% pull(por), color = "#7FDB85") |>
  hc_xAxis(categories = unique(as.character(df$var))) |> 
  hc_yAxis(labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
  hc_plotOptions(
    series = list(stacking = "percent"),
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        pointWidth = 3,
        formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
      )
    )
  ) |> 
  hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
  hc_legend(enabled = TRUE) |> 
  hc_exporting(enabled = TRUE) 
}

graf_seguvoto_alt <- function(df, titulo, subtitulo) {
  highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = titulo, style = list(fontWeight = "bold")) |> 
    hc_subtitle(text = subtitulo) |> 
    hc_add_series(name = "Ns/Nc", data = df %>% filter(opinion == "Ns/Nc") %>% pull(por), color = "gray60") |>
    hc_add_series(name = "Poco seguro", data = df %>% filter(opinion == "Poco seguro") %>% pull(por), color = "#D97F7F") |>
    hc_add_series(name = "Algo seguro", data = df %>% filter(opinion == "Algo seguro") %>% pull(por), color = "orange") |>
    hc_add_series(name = "Muy seguro", data = df %>% filter(opinion == "Muy seguro") %>% pull(por), color = "#7FDB85") |>
    hc_xAxis(categories = unique(as.character(df$var))) |> 
    hc_yAxis(labels = list(formatter = JS("function() { return (this.value * 100).toFixed(0) + '%'; }"))) |> 
    hc_plotOptions(
      series = list(stacking = "percent"),
      bar = list(
        dataLabels = list(
          enabled = TRUE,
          pointWidth = 3,
          formatter = JS("function() { return (this.y * 100).toFixed(1) + '%'; }")
        )
      )
    ) |> 
    hc_tooltip(formatter = JS("function() { return '<b>' + this.x + '</b><br/>' + this.series.name + ': ' + (this.y * 100).toFixed(1) + '%'; }")) |> 
    hc_legend(enabled = TRUE) |> 
    hc_exporting(enabled = TRUE) 
}
