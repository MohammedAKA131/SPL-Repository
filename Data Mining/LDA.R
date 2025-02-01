library(topicmodels)

# افترض أن لدينا مصفوفة الوثائق-المصطلحات dtm ونريد إنشاء نموذج LDA بعدد مواضيع معين k
k <- 5  # على سبيل المثال 5 مواضيع
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# حساب perplexity للنموذج
model_perplexity <- perplexity(lda_model, dtm)
cat("Perplexity:", model_perplexity, "\n")

# حساب log-likelihood للنموذج
model_logLik <- logLik(lda_model)
cat("Log-Likelihood:", model_logLik, "\n")
