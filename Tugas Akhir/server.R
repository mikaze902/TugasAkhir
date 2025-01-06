server <- function(input, output) {
  
  ##Import data
  data_ku <- reactive({
    
    ambil_file_data <- input$ambil_file_data
    
    if(is.null(ambil_file_data))
      return(NULL)
    
    p = read.csv(ambil_file_data$datapath, sep = input$pemisah_variabel)
    
    return(p)
  })
  
  output$tampilkan_data <- DT::renderDT({
    
    ambil_data <- data_ku()
    
    DT::datatable(ambil_data)
  })
  
  ##Select Variable
  kirim_pilih_variabel <- reactive({
    
    indeks_pilihan_variabel <- read.csv(text = input$pilih_variabel, header = F,
                                        sep = "", na.strings = c("", "NA", "."))
    
    indeks_pilihan_variabel = unlist(indeks_pilihan_variabel)
    
    indeks_pilihan_variabel = as.numeric(indeks_pilihan_variabel)
    
    return(indeks_pilihan_variabel)
  })
  output$pilihan_variabel_anda <- renderPrint({
    
    ambil_data <- data_ku()
    
    nama_variabel <- colnames(ambil_data)
    
    indeks_pilih_variabel <- kirim_pilih_variabel()
    
    cat(sprintf("Selected variable(s):\n\n"))
    
    print(nama_variabel[c(indeks_pilih_variabel)])
  })
  
  
  ##Hasil Korelasi
  
  output$hasil_korelasi_pearson <- DT::renderDT({
    
    ambil_data <- data_ku()
    
    indeks_pilih_variabel <- kirim_pilih_variabel()
    
    hasil_perhitungan_korelasi <- hitung_korelasi(ambil_data[indeks_pilih_variabel], 
                                                  input$setting_desimal)
    
    DT::datatable(hasil_perhitungan_korelasi[[1]])
  })
  
  
  
  print("hello world"),
  }
  
  
  
}
