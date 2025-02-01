# تحميل المكتبات اللازمة
library(tm)
library(topicmodels)
library(stopwords)
library(readtext)
library(stringr)
# يمكنك البحث عن حزمة تدعم التجذير العربي أو إعداد دالة خاصة للتجذير

# دالة لإزالة التشكيل وتوحيد بعض الحروف (يمكن تحسينها بحسب الحاجة)
normalize_arabic <- function(text) {
  text <- str_replace_all(text, "[ًٌٍَُِّْـ]", "")         # إزالة التشكيل
  text <- str_replace_all(text, "[إأآا]", "ا")              # توحيد الألف
  text <- str_replace_all(text, "ى", "ي")
  text <- str_replace_all(text, "ؤ", "و")
  text <- str_replace_all(text, "ئ", "ي")
  return(text)
}

# تحميل البيانات من ملف الوورد
file_path <- "C:/Users/AhmedMohammedAAlzaid/Desktop/SPLPROJECT.docx"
text_data <- readtext(file_path)$text

# تقسيم المقالات باستخدام "###" كفاصل
articles <- unlist(strsplit(text_data, "###"))

# تطبيق التوحيد وتنظيف النص الأساسي
articles <- tolower(articles)
articles <- normalize_arabic(articles)

# إزالة علامات الترقيم والأرقام
articles <- removePunctuation(articles)
articles <- removeNumbers(articles)

# تحديث قائمة stopwords العربية (يمكنك توسيع القائمة أو استخدام قائمة جاهزة)
arabic_stopwords <- stopwords("ar", source = "misc")
custom_stopwords <- c("الأندية", "السعودية", "الدوري", "المباراة", "الكرة", "الفريق", "الموسم", 
                      "المباريات", "نادي", "النادي", "اللاعب", "الفرق", "اللقاء", "الرياضة",
                      "البطولات", "الترتيب", "المراكز", "النتائج", "الصعود", "الدرجة", "الكبرى")
all_stopwords <- unique(c(arabic_stopwords, custom_stopwords))

articles <- removeWords(articles, all_stopwords)
articles <- stripWhitespace(articles)

# إنشاء الكوربوس
corpus <- Corpus(VectorSource(articles))

# إنشاء مصفوفة المصطلحات
dtm <- DocumentTermMatrix(corpus)

# التأكد من وجود بيانات في المصفوفة
if (nrow(dtm) == 0 || ncol(dtm) == 0) {
  stop("⚠️ خطأ: لا يوجد بيانات كافية في DTM. تحقق من محتوى المقالات بعد التنظيف.")
}

# حساب وزن TF-IDF على DTM
dtm_tfidf <- weightTfIdf(dtm)

# تحويل المصفوفة إلى مصفوفة قابلة للتحليل
dtm_matrix <- as.matrix(dtm_tfidf)
words <- colnames(dtm_matrix)

# قائمة أسماء الأندية السعودية
teams <- c("الهلال", "الاتحاد", "النصر", "الأهلي", "الشباب", "الاتفاق", "الرائد",
           "الفتح", "الوحدة", "الطائي", "الحزم", "الخليج", "الفيحاء", "أبها", "ضمك", "القادسية")

# إنشاء جدول لحفظ النتائج
team_word_freq <- data.frame(Team = character(), Word = character(), TFIDF = numeric(), stringsAsFactors = FALSE)

# استخراج الكلمة الأكثر ارتباطًا لكل فريق باستخدام TF-IDF
for (team in teams) {
  if (team %in% words) {
    team_index <- which(words == team)
    
    # تحديد المقالات التي تحتوي على ذكر الفريق (حسب DTM الأصلية، يمكن استخدام dtm_matrix هنا أيضاً)
    dtm_binary <- as.matrix(dtm)  # استخدام تكرار الكلمات الثابت لتحديد المقالات
    relevant_articles <- which(dtm_binary[, team_index] > 0)
    
    if (length(relevant_articles) > 0) {
      # استخراج مصفوفة TF-IDF للمقالات ذات العلاقة بالفريق
      team_tfidf <- dtm_matrix[relevant_articles, , drop = FALSE]
      
      # حساب المتوسط لكل كلمة عبر المقالات الخاصة بالفريق
      word_tfidf_avg <- colMeans(team_tfidf)
      
      # استبعاد اسم الفريق وأسماء الفرق الأخرى والكلمات العامة
      exclude_words <- unique(c(team, teams, all_stopwords))
      word_tfidf_avg <- word_tfidf_avg[!names(word_tfidf_avg) %in% exclude_words]
      
      # اختيار الكلمة ذات أعلى قيمة TF-IDF والتي تزيد عن حد معين (مثلاً > 0)
      valid_words <- word_tfidf_avg[word_tfidf_avg > 0]
      if (length(valid_words) > 0) {
        most_associated_word <- names(sort(valid_words, decreasing = TRUE))[1]
        max_tfidf <- valid_words[most_associated_word]
        
        # حفظ النتيجة
        team_word_freq <- rbind(team_word_freq, 
                                data.frame(Team = team, Word = most_associated_word, TFIDF = max_tfidf))
      }
    }
  }
}

# إعادة تعيين أسماء الصفوف (للتخلص من الأسماء غير المرغوب فيها)
rownames(team_word_freq) <- NULL

# طباعة النتائج
if (nrow(team_word_freq) > 0) {
  print("🔹 أكثر كلمة مرتبطة بكل نادٍ في الدوري السعودي عبر المقالات (باستخدام TF-IDF):")
  print(team_word_freq)
} else {
  print("⚠️ لم يتم العثور على كلمات مرتبطة بالأندية. تحقق من البيانات بعد التنظيف.")
}
