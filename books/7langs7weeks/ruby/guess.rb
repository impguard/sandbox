max_value = 10

if ARGV.length > 0
    max_value = ARGV[0].to_i
end

cold_range = max_value * 0.1
hot_range = max_value * 0.4
number = rand(max_value)
guess = -1

random_messages = [
    "Darn, try again.",
    "Nope, not there yet.",
    "Nah.",
    "Nope.",
    "Nuh-uh",
    "Nein!",
    "Eh. Nooope.",
    "Are you getting bored yet?",
    "Man, you're trying so hard!",
    "Sorry, incorrect.",
    "You can do it! If you keep trying...",
    "Why would you guess %{guess}, why!!?!"
]

random_hotter_messages = [
    "Close!",
    "Almost there...",
    "I mean, give or take some and you'll be there.",
    "The closer they get the harder they fall. Wait, is that right?",
    "Ooooooooooh. Nope.",
    "Whoo, I almost thought we were done here.",
    "Do you really want this to end?",
    "Mmmm, I could almost give it to you."
]

random_colder_messages = [
    "Uh. Not even close, babyyy!",
    "Are you even trying?",
    "It's not like this game takes skill or anything.",
    "Alright, I may need to give you 1000 guesses.",
    "I got one number and %{guess} ain't one!"
]

puts "Guess a number between 1 and #{max_value}!"
while true
    print "What's your guess? "
    guess = STDIN.gets.to_i

    if guess == number
        puts "You got it! We have a winner!"
        break
    end

    msg_params = {:guess => guess}

    if (guess - number).abs <= cold_range
        puts random_hotter_messages.sample % msg_params
    elsif (guess - number).abs >= hot_range
        puts random_colder_messages.sample % msg_params
    else
        puts random_messages.sample % msg_params
    end
end
