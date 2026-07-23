## Pertemuan 4: Visualisasi Lanjutan ##
# Pasca memahami beberapa perintah dasar macam ggplot, aes mapping serta geom point, bahkan pada konsep labs untuk label visualisasi yang jauh lebih rapi
# kita akan mengeksplorasi lebih jauh beberapa konsep visualisasi lainnya
# Mari masuk ke Data Visualization Lanjutan 

library(gapminder)
library(tidyverse)
library(ggplot2)
library(dplyr)
gapminder
View(gapminder)

# Pada pertemuan kali ini kita akan masuk ke konsep faceting & grouping
# Kita akan mengambil sampel data yang ingin kita olah, i.e gapminder pada setiap data tahun 2007

gap07 <- gapminder |> filter(year == 2007)
ggplot(gap07, aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_point(size = 2.5, alpha = 0.7) + 
  labs(title = "Pendapatan vs Harapan Hidup (2007)", x = "Log PDB per Kapita",
       y = "Harapan Hidup", color = "Benua") # Gunakan labs untuk memberikan label pada title, xlab, ylab dan keterangan lainnya

# Kita masuk untuk memahami facet_wrap() kita coba start pecah plotnya per-Benua

ggplot(gap07, aes(x = log(gdpPercap), y = lifeExp, color = continent)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth (method = "lm", se = FALSE, color = "black", linetype = "dashed") + # se merupakan standard error (diberi false agar confidence interval hilang)
  facet_wrap(~ continent, ncol = 3) + # ncol digunakan untuk mengatur layout faset menjadi 3 kolom
  labs(title = "Hubungan PDB & Harapan Hidup per Benua",
       x = "Gdp per Kapita Benua", y = "Harapan Hidup") 

# Lanjut kita masuk mempelajari facet_grid(), digunakan untuk membandingkan tren pada tahun serta benua tertentu
# Kita ambil data tahun 2007 vs 1952 pada benua amerika, afrika dan asia 

gap_comparison <- gapminder |> filter(year %in% c(1952, 2007),
                                      continent %in% c("Asia", "Americas", "Africa"))

# Mulai start bangun plotting data tadi 

ggplot(gap_comparison, aes(x = log(gdpPercap), y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.7) + 
  facet_grid(year ~ continent) + # Tahun dalam bentuk baris dan Benua dalam bentuk kolom
  labs(title = "Evolusi PDB vs Harapan Hidup (1952 vs 2007)",
       x = "PDB per Kapita",
       y = "Angka Harapan Hidup")

# Dapat dilihat bagaimana visualisasi dari evolusi PDB vs Harapan Hidup yang sudah kita bangun sedemikian rupa
# Dapat kita tambahkan beberapa instruksi lebih lanjut terkait masalah kustomisasi tema dan skala
# Mari kita tambahkan sintaks dari pasca facet_grid tadi
    
ggplot(gap_comparison, aes(x = log(gdpPercap), y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.7) + 
  facet_grid(year ~ continent) + # Tahun dalam bentuk baris dan Benua dalam bentuk kolom
  labs(title = "Evolusi PDB vs Harapan Hidup (1952 vs 2007)",
       x = "PDB per Kapita",
       y = "Angka Harapan Hidup") + 
  scale_color_brewer(palette = "Set1") + # Dapat kita pakai untuk mengubah palet bawaan menjadi warna yang lebih elegan 
  theme_bw() + # untuk menghapus background abu abu menjadi putih bersih
  theme(plot.title = element_text(face = "bold", size = 14), # theme digunakan untuk memberikan tema pada data tsb
        strip.background = element_rect(fill = "gray90"),
        panel.grid.minor = element_blank()
        )
  
# Selesai, hasil kodingan selanjutnya merupakan bentuk eksplorasi mandiri pada dataset gapminder
# Mari kita lihat bagaimana perbandingan data PDB per Kapita Indonesia & Life Expectancy tahun 1952 s.d 2007

# Bangun data yang ingin coba kita filter yakni khusus country Indonesia (709 s.d 720 pada gapminder)

Indonesia_linimasa <- gapminder |> filter(year %in% c(1952, 1957, 1962, 1967, 1972, 1977,
                                                      1982, 1987, 1992, 1997, 2002, 2007),
                                          country %in% "Indonesia")

# Kita coba visualisasikan dengan ggplot                                          

ggplot(Indonesia_linimasa, aes(x = log(gdpPercap), y = lifeExp)) + 
  geom_point(size = 2.5, alpha = 0.8) + 
  labs(title = "GDP per Kapita & Life Expectancy Indonesia (1952-2007)",
       x = "PDB per Kapita",
       y = "Angka Harapan Hidup")

# Great, no Error, but too "Flat"
# Coba kita desain sedemikian rupa agar berbentuk mirip dengan eksplorasi data kita yang terakhir

ggplot(Indonesia_linimasa, aes(x = log(gdpPercap), y = lifeExp)) + 
  geom_point(size = 2.5, alpha = 0.8, color = "red") + 
  geom_smooth(method = "lm", color = "blue", linetype = "longdash") +
  labs(title = "GDP per Kapita & Life Expectancy Indonesia (1952-2007)",
       x = "Log PDB per Kapita",
       y = "Angka Harapan Hidup") + 
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 14), 
  strip.background = element_rect(fill = "gray90"),
  panel.grid.minor = element_blank()) + 
  
# Coba kita cari lebih lanjut mengenai korelasi data antara PDB per Kapita dengan Angka Harapan Hidup tersebut

cor(log(Indonesia_linimasa$gdpPercap), Indonesia_linimasa$lifeExp)

# Jika dilaunch, maka menghasilkan nilai korelasi (pearson) yang sangat kuat 
# r = 0.9760688...
# Selesai