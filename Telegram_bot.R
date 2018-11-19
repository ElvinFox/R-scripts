user_renviron <- path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron) 

#   bot name -> TheOneForTest_bot
#   chat_id -> 279836128
#   R_TELEGRAM_BOT_TheOneForTest_bot = 

# install.packages("telegram.bot")
library(telegram.bot)

chat_id <- 279836128

bot <- Bot(token = bot_token('TheOneForTest_bot'))
#updates <- bot$getUpdates()
print(bot$getMe())


#sending text message
message_to_bot=sprintf('Process finished - Accuracy: %s', 0.1)
bot$sendMessage(chat_id = chat_id, text = message_to_bot)

#sending picture
library(ggplot2)
my_plot=ggplot(mtcars, aes(x=mpg))  + geom_histogram(bins = 5)
ggsave("my_plot.png", my_plot)

bot$sendPhoto(chat_id = chat_id, photo = 'my_plot.png')



for (i in 1:100) {
    if( (i %% 30) == 0 )     bot$sendMessage(chat_id = chat_id, text = paste0("Done, last number is ", i))
    if( (i %% 30) == 0 )     Sys.sleep(10)
    if(i == 100) bot$sendMessage(chat_id = chat_id, text = "Done")
    bot$sendAnimation(chat_id = chat_id,
                      animation = "https://media.giphy.com/media/sIIhZliB2McAo/giphy.gif")
    bot$send_document( chat_id = chat_id, document = 'market_review.html')
}
