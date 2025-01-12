# R course for beginners
# Week 9
# Asaf Hoory 211671714

#Generating data frame
N = 50
df <- data.frame(
  age = runif(N, min = 18, max = 60),
  gender = factor(sample(c('male', 'female'),size = N, replace = TRUE), levels = c('male', 'female')),
  RT = runif(N, min = 200, max = 6000),
  depression = runif(N, min = 0, max = 100),
  sleep_duration = runif(N, min = 2, max = 12)
)



conditional_multiply <- function(x1, x2) {
  if( x1 > 5 & x2 > 5) {
    result <- (x1 * x2) 
  } else {
    result <- (x1 + x2) 
  }
  return(result)
}

conditional_multiply(5,6)
conditional_multiply(6,6)


# פונקציה שמכפילה שני מספרים אם הם גדולים מ-5, אחרת מחברת אותם
conditional_multiply <- function(x, y) {
  if (x > 5 & y > 5) {
    result <- x * y  # פעולה: הכפלה אם שני המספרים גדולים מ-5
  } else {
    result <- x + y  # פעולה: חיבור אם אחד או שניהם קטנים או שווים ל-5
  }
  return(result)  # הפלט: תוצאה בהתאם לתנאי
}

# דוגמה לשימוש בפונקציה
conditional_multiply(6, 7)  # הפלט יהיה 42 (כפול)
conditional_multiply(4, 7)  # הפלט יהיה 11 (חיבור)


