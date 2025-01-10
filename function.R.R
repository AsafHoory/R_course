descriptive_stat <- function(data, subject_start, subject_end) {
  # בדיקת מספר התצפיות
  if (nrow(data) < 10) {
    stop("data is too short")  # הפסקת הפונקציה עם הודעת שגיאה
  }
  
  # סינון הנתונים לפי טווח הנבדקים
  data <- data[data$subject_id >= subject_start & data$subject_id <= subject_end, ]
  
  # בדיקה אם לאחר הסינון נשארו תצפיות
  if (nrow(data) == 0) {
    stop("No data in the specified range of subjects")  # הודעת שגיאה אם אין תצפיות בטווח
  }
  
  # יצירת Data Frame ריק עבור התוצאות
  results <- data.frame(
    variable = character(),
    type = character(),
    mean = numeric(),
    min = numeric(),
    max = numeric(),
    levels = character(),
    stringsAsFactors = FALSE
  )
  
  # חישוב הסטטיסטיקה התאורית
  for (var_name in names(data)) {
    var <- data[[var_name]]
    
    if (class(var) %in% c("numeric", "integer")) {
      results <- rbind(results, data.frame(
        variable = var_name,
        type = "numeric",
        mean = mean(var),
        min = min(var),
        max = max(var),
        levels = NA
      ))
    } else if (class(var) %in% c("factor", "character")) {
      levels_counts <- paste(names(table(var)), table(var), sep = ": ", collapse = ", ")
      results <- rbind(results, data.frame(
        variable = var_name,
        type = "categorical",
        mean = NA,
        min = NA,
        max = NA,
        levels = levels_counts
      ))
    }
  }
  
  # החזרת התוצאות
  return(results)
}

# דוגמה לשימוש בפונקציה:
# סטטיסטיקה רק עבור נבדקים 5 עד 10
results <- descriptive_stat(data = df, subject_start = 5, subject_end = 10)
print(results)
