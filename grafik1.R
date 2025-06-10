library(readr)
library(readxl)

veri <- read_excel("C:/Users/iremt/OneDrive/Desktop/StudentsPerformance.xlsx")

class(veri)
names(veri)
head(veri)
# Kategorik degiskenler
categorical_vars <- veri[, c("gender", 
                             "race_ethnicity", 
                             "parental_level_of_education", 
                             "lunch", 
                             "test_preparation_course")] |> 
  lapply(as.factor) |> 
  as.data.frame()

# Surekli degiskenler
continuous_vars <- veri[, c("math_score", 
                            "reading_score", 
                            "writing_score")]


summary(veri)



library(psych)    
library(moments)  

ozet_istatistik <- function(x) {
  n <- length(x)
  ort <- mean(x)
  sd <- sd(x)
  medyan <- median(x)
  minv <- min(x)
  maks <- max(x)
  aralik <- maks - minv
  carpiklik <- skewness(x)
  basiklik <- kurtosis(x)
  se <- sd / sqrt(n)
  
  cat("n:", n, "\n")
  cat("Ortalama:", round(ort, 2), "\n")
  cat("Standart Sapma (SD):", round(sd, 2), "\n")
  cat("Medyan:", medyan, "\n")
  cat("Minimum:", minv, "\n")
  cat("Maksimum:", maks, "\n")
  cat("Aral??k:", aralik, "\n")
  cat("??arp??kl??k (Skewness):", round(carpiklik, 2), "\n")
  cat("Bas??kl??k (Kurtosis):", round(basiklik, 2), "\n")
  cat("Standart Hata (SE):", round(se, 2), "\n")
}
ozet_istatistik(veri$math_score)
ozet_istatistik(veri$reading_score)
ozet_istatistik(veri$writing_score)



library(corrplot)
library(RColorBrewer)
library(ggcorrplot)
library(corrplot)




# Kursa katilanlar ve katilmayanlari ayir
grup1 <- veri[veri$test_preparation_course == "completed", c("math_score", "reading_score", "writing_score")]
grup2 <- veri[veri$test_preparation_course == "none", c("math_score", "reading_score", "writing_score")]

# Korelasyonlar?? hesapla
cor_grup1 <- cor(grup1)
cor_grup2 <- cor(grup2)

# Kursu tamamlayanlar i??in korelasyon grafi??i
ggcorrplot(cor_grup1,
           method = "circle",
           type = "upper",
           lab = TRUE,
           lab_size = 4,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Kursu Tamamlayanlar: Notlar Aras?? Korelasyon",
           ggtheme = ggplot2::theme_minimal())

# Kursa kat??lmayanlar i??in korelasyon grafi??i
ggcorrplot(cor_grup2,
           method = "circle",
           type = "upper",
           lab = TRUE,
           lab_size = 4,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Kursa Katilmayanlar: Notlar Arasi Korelasyon",
           ggtheme = ggplot2::theme_minimal())





# Gerekli paketler
if (!require("DescTools")) install.packages("DescTools")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")

library(DescTools)
library(ggplot2)
library(tidyr)
library(dplyr)

# Fonksiyon: Eta Kare Hesaplama
eta_kare_hesapla <- function(veri, kategorik_deg, puanlar) {
  sonuclar_list <- list()
  index <- 1
  
  for (cat_var in kategorik_deg) {
    for (puan in puanlar) {
      form <- as.formula(paste(puan, "~", cat_var))
      eta <- EtaSq(aov(form, data = veri))[1]
      
      yeni_satir <- list(
        Kategorik = cat_var,
        Skor = puan,
        Eta_Square = round(eta, 3)
      )
      
      sonuclar_list[[index]] <- yeni_satir
      index <- index + 1
    }
  }
  
  # DataFrame'e gevir
  sonuclar_df <- do.call(rbind, lapply(sonuclar_list, as.data.frame))
  return(sonuclar_df)
}

# Kategorik depi~kenler ve puanlar
kategorik_deg <- c("gender", "race_ethnicity", "parental_level_of_education", "lunch", "test_preparation_course")
puanlar <- c("math_score", "reading_score", "writing_score")



# Eta kare deperlerini hesapla
sonuclar_df <- eta_kare_hesapla(veri, kategorik_deg, puanlar)

# Gvrselle~tirme: Is} Haritas} (Heatmap)
ggplot(sonuclar_df, aes(x = Skor, y = Kategorik, fill = Eta_Square)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Kategorik Depi~kenler ile Puanlar Aras}ndaki Etki (Eta Kare)",
    x = "Puan Turu",
    y = "Kategorik Degi~ken",
    fill = "Eta Kare"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Her bir degisken iC'in Shapiro-Wilk testi
shapiro.test(veri$math_score)
shapiro.test(veri$reading_score)
shapiro.test(veri$writing_score)
degiskenler <- veri[, c("math_score", "reading_score", "writing_score")]

sonuclar <- lapply(degiskenler, shapiro.test)

# p-degerlerini ve test istatistiklerini daha okunabilir hale getirme
sapply(sonuclar, function(x) c(W_statistic = round(x$statistic, 3), p_value = round(x$p.value, 4)))




##----------------------------------------------cok sutunlu grafik-------------

install.packages(c("ggplot2", "ggdist", "gghalves", "viridis", "patchwork"))
library(ggplot2)
library(ggdist)
library(gghalves)
library(viridis)
library(patchwork)  


create_raincloud_plot <- function(df, score_var, score_label) {
  ggplot(df, aes(x = test_preparation_course, y = .data[[score_var]], fill = test_preparation_course)) +
    
    # Yogunluk (density)
    stat_halfeye(
      adjust = 0.6,
      width = 0.7,
      .width = 0,
      justification = -0.15,
      point_colour = NA,
      alpha = 0.7
    ) +
    
    # Puan noktalar (rain)
    geom_half_point(
      side = "l",
      range_scale = 0.5,
      alpha = 0.5,
      shape = 21,
      fill = "#E6E6FA",
      color = "black",
      stroke = 0.2,
      size = 2.0
    ) +
    
    # Boxplot
    geom_boxplot(
      width = 0.15,
      outlier.shape = NA,
      alpha = 0.25,
      color = "gray20"
    ) +
    
    scale_fill_manual(values = c("#DDA0DD", "#87CEEB")) +  # Canl?? ve a????k pembe tonlar??
    
    labs(
      x = "Haz??rl??k Kursu Durumu",
      y = score_label
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(color = "gray29"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}


p1 <- create_raincloud_plot(veri, "math_score", "Matematik Skoru")
p2 <- create_raincloud_plot(veri, "reading_score", "Okuma Skoru")
p3 <- create_raincloud_plot(veri, "writing_score", "Yazma Skoru")


raincloud_combined <- (p1 | p2 | p3) + plot_annotation(title = "Hazirlik Kursu Gruplarina Gore Skorlarin Raincloud Grafikleri")

ggsave("raincloud_scores_all.png", raincloud_combined, width = 14, height = 6, dpi = 300)


print(raincloud_combined)




# t-testleri
t_math <- t.test(math_score ~ test_preparation_course, data = veri)
t_reading <- t.test(reading_score ~ test_preparation_course, data = veri)
t_writing <- t.test(writing_score ~ test_preparation_course, data = veri)

# Sonuglar}n yazd}r}lmas}
t_math
t_reading
t_writing



t.test(math_score ~ test_preparation_course, data = veri)
t.test(reading_score ~ test_preparation_course, data = veri)
t.test(writing_score ~ test_preparation_course, data = veri)





lm(math_score ~ test_preparation_course + gender + parental_level_of_education + lunch, data = veri)
lm(reading_score ~ test_preparation_course + gender + parental_level_of_education + lunch, data = veri)
lm(writing_score ~ test_preparation_course + gender + parental_level_of_education + lunch, data = veri)







##kumeleme ve buna g??re raincloud

# Standartlastirma 
veri_scaled <- scale(continuous_vars)
# Kumeleme 
set.seed(123)
kmeans_result <- kmeans(veri_scaled, centers = 3, nstart = 25)

# Kume  veri setine ekleyelim
veri$kume <- as.factor(kmeans_result$cluster)

# Yeni fonksiyon: Kume bilgisine gore Raincloud Grafigi
create_clustered_raincloud_plot <- function(df, score_var, score_label) {
  ggplot(df, aes(x = test_preparation_course, y = .data[[score_var]], fill = kume)) +
    
    # Yogunluk (density)
    stat_halfeye(
      adjust = 0.6,
      width = 0.7,
      .width = 0,
      justification = -0.15,
      point_colour = NA,
      alpha = 0.7
    ) +
    
    # Nokta (rain)
    geom_half_point(
      side = "l",
      range_scale = 0.5,
      alpha = 0.5,
      shape = 21,
      aes(fill = kume, group = test_preparation_course),
      color = "black",
      stroke = 0.2,
      size = 2.0
    ) +
    
    
    # Boxplot
    geom_boxplot(
      width = 0.15,
      outlier.shape = NA,
      alpha = 0.25,
      color = "gray20"
    ) +
    
    scale_fill_viridis_d(option = "D") +
    
    labs(
      x = "K??me Numaras?? (Ba??ar?? Grubu)",
      y = score_label
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.text = element_text(color = "gray29"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}

p1 <- create_clustered_raincloud_plot(veri, "math_score", "Matematik Skoru")
p2 <- create_clustered_raincloud_plot(veri, "reading_score", "Okuma Skoru")
p3 <- create_clustered_raincloud_plot(veri, "writing_score", "Yazma Skoru")

# Hepsini yan yana birlestir
raincloud_clustered <- (p1 | p2 | p3) + 
  plot_annotation(title = "Hazirlik Kursuna ve Kumeye Gore Basari Dagilimi")


ggsave("raincloud_scores_clustered.png", raincloud_clustered, width = 14, height = 6, dpi = 300)


print(raincloud_clustered)





# Jitter plot, renk cinsiyete gore
install.packages("tidyr")  
library(tidyr)             


library(ggplot2)
library(dplyr)
library(extrafont)  

veri <- veri %>%
  mutate(parental_level_of_education = factor(
    parental_level_of_education,
    levels = c(
      "some high school",
      "high school",
      "some college",
      "associate's degree",
      "bachelor's degree",
      "master's degree"
    )
  ))

# Jitter plot
ggplot(veri, aes(x = parental_level_of_education, y = writing_score, color = gender)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, color = "red4", fatten = 1.2, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  
  labs(
    title = "Ebeveyn Egitim Duzeyi Cinsiyete Gore Yazma Puanlari",
    x = "Ebeveyn Egitim Duzeyi",
    y = "Yazma Puani",
    color = "Cinsiyet"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +  # Arial genellikle T??rk??e karakterleri destekler
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )







veri %>%
  group_by(parental_level_of_education, gender) %>%
  summarise(mean_score = mean(writing_score))

anova_model <- aov(writing_score ~ parental_level_of_education* gender, data = veri)
summary(anova_model)
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)





###-------------------------kumeleme-------------------------------------------------


library(dplyr)

edu_means <- veri %>%
  group_by(parental_level_of_education) %>%
  summarise(
    math = mean(math_score),
    reading = mean(reading_score),
    writing = mean(writing_score)
  )

# Sonuglar} gvrmek igin
print(edu_means)

# Kendi olu~turdupun k|meyi dataframe olarak tan}mla:
egitim_kume <- data.frame(
  parental_level_of_education = c("some high school", "high school", "some college",
                                  "associate's degree", "bachelor's degree", "master's degree"),
  egitim_kumesi = as.factor(c(1, 1, 2, 2, 3, 3))  # Buray} sen kendi k|me karar}na gvre depi~tir
)
veri <- left_join(veri, egitim_kume, by = "parental_level_of_education")

# ANOVA modeli
anova_model2 <- aov(writing_score ~ egitim_kumesi* gender, data = veri)
summary(anova_model2)
# Tukey HSD testi
tukey_result2 <- TukeyHSD(anova_model2)
print(tukey_result2)


library(ggplot2)

ggplot(veri, aes(x = egitim_kumesi, y = writing_score, color = gender)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "black", fatten = 2) +
  labs(
    title = "Ebeveyn Egitim Kumesine Gore Yazma Skoru Dagilimi",
    x = "Egitim Kumesi",
    y = "Yazma Skoru",
    color = "Cinsiyet"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



