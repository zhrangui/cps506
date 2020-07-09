subset_mtcars <- mtcars[,c("mpg","disp","hp","wt")]
head(subset_mtcars)
plot(subset_mtcars)
full = lm ( mpg ~ disp + hp + wt , data = mtcars )
reduced = lm (mpg ~ disp , data = mtcars )
anova(reduced, full)
