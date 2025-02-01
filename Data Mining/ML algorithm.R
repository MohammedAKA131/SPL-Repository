# تحميل المكتبات اللازمة
library(tm)
library(topicmodels)
library(stopwords)
library(readtext)
library(SnowballC)
library(stringr)

# تحميل البيانات من ملف الوورد
file_path <- "C:/Users/AhmedMohammedAAlzaid/Desktop/SPLPROJECT.docx"
text_data <- readtext(file_path)$text

# تقسيم المقالات باستخدام "###" كفاصل
articles <- unlist(strsplit(text_data, "###"))  # تقسيم النص إلى مقالات

# قائمة أسماء الأندية السعودية
teams <- c("الهلال", "الاتحاد", "النصر", "الأهلي", "الشباب", "الاتفاق", "الرائد",
           "الفتح", "الوحدة", "الطائي", "الحزم", "الخليج", "الفيحاء", "أبها", "ضمك", "القادسية")

# قائمة الكلمات العامة التي يجب استبعادها
custom_stopwords <- c("الأندية", "السعودية", "الدوري", "المباراة", "الكرة", "الفريق", "الموسم", 
                      "المباريات", "نادي", "النادي", "اللاعب", "الفرق", "اللقاء", "الرياضة",
                      "البطولات", "الترتيب", "المراكز", "النتائج", "الصعود", "الدرجة", "الكبرى")

# تنظيف النصوص لكل مقالة
clean_articles <- tolower(articles)  # تحويل إلى حروف صغيرة
clean_articles <- removePunctuation(clean_articles)  # إزالة علامات الترقيم
clean_articles <- removeNumbers(clean_articles)  # إزالة الأرقام
clean_articles <- removeWords(clean_articles, c(stopwords("ar", source = "misc"), custom_stopwords))  # تحديث stopwords
clean_articles <- stripWhitespace(clean_articles)  # إزالة الفراغات الزائدة

# إنشاء الكوربوس لكل مقالة كمستند منفصل
corpus <- Corpus(VectorSource(clean_articles))

# تحويل النصوص إلى مصفوفة المصطلحات (DTM)
dtm <- DocumentTermMatrix(corpus)

# التأكد أن DTM ليس فارغًا
if (nrow(dtm) == 0 || ncol(dtm) == 0) {
  stop("⚠️ خطأ: لا يوجد بيانات كافية في DTM. تحقق من محتوى المقالات بعد التنظيف.")
}

# تحويل المصفوفة إلى مصفوفة قابلة للتحليل
dtm_matrix <- as.matrix(dtm)
words <- colnames(dtm_matrix)

# إنشاء جدول لحفظ النتائج
team_word_freq <- data.frame(Team = character(), Word = character(), Frequency = integer(), stringsAsFactors = FALSE)

# البحث عن الكلمة الأكثر ارتباطًا بكل فريق عبر المقالات التي ذكر فيها الفريق فقط
for (team in teams) {
  if (team %in% words) {
    team_index <- which(words == team)
    
    # تحديد المقالات التي تحتوي على الفريق
    relevant_articles <- which(dtm_matrix[, team_index] > 0)
    
    if (length(relevant_articles) > 0) {
      # تحليل الكلمات داخل المقالات التي تحتوي على الفريق
      word_frequencies <- colSums(dtm_matrix[relevant_articles, , drop = FALSE])
      
      # استبعاد اسم الفريق نفسه والكلمات العامة
      word_frequencies <- word_frequencies[!names(word_frequencies) %in% c(team, teams, custom_stopwords, stopwords("ar", source = "misc"))]
      
      # اختيار الكلمات التي لا تظهر مع أكثر من فريق بنفس التكرار
      unique_words <- names(word_frequencies)[!duplicated(word_frequencies)]
      word_frequencies <- word_frequencies[unique_words]
      
      # اختيار الكلمة الأكثر تكرارًا في سياق كل فريق مع شرط أن يكون تكرارها على الأقل 2
      valid_words <- word_frequencies[word_frequencies >= 2]
      if (length(valid_words) > 0) {
        most_associated_word <- names(sort(valid_words, decreasing = TRUE))[1]
        freq <- valid_words[most_associated_word]
        
        # حفظ النتيجة
        team_word_freq <- rbind(team_word_freq, data.frame(Team = team, Word = most_associated_word, Frequency = freq))
      }
    }
  }
}

# طباعة النتائج
if (nrow(team_word_freq) > 0) {
  print("🔹 أكثر كلمة مرتبطة بكل نادٍ في الدوري السعودي عبر المقالات:")
  print(team_word_freq)
} else {
  print("⚠️ لم يتم العثور على كلمات مرتبطة بالأندية. تحقق من البيانات بعد التنظيف.")
}

