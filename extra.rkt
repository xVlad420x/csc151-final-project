#lang racket

(require csc151)

(define adjectives-old
  (list "inescapable"
        "dark"
        "depressing"
        "downward"
        "unathletic"
        "stupid"
        "athletic"
        "smart"
        "correct"
        "strange"
        "gigantic"
        "cracked"
        "scattered"
        "triggered"
        "uneventful"
        "different"
        "automatic"
        "futuristic"
        "white"
        "old"
        "technological"
        "able"
        "greatest"
        "capable"
        "great"
        "impressed"
        "confused"
        "oppressive"
        "scattered"
        "luckily"
        "swirling"
        "horrible"
        "best"
        "given"
        "upload"
        "angrier"))
        
(define verbs-old
  (list "materialize"
        "escape"
        "begin"
        "continue"
        "clear"
        "land"
        "accelerate"
        "dodge"
        "weave"
        "decide"
        "place"
        "find"
        "sort"
        "add"
        "play"
        "solve"
        "choose"
        "step"
        "warp"
        "realize"
        "hit"
        "step"
        "think"
        "see"))

(define nouns-old
  (list "basement"
        "soul"
        "roof"
        "sword"
        "bow"
        "spear"
        "force"
        "doofus"
        "speed"
        "reality"
        "containers"
        "category"
        "tanker"
        "floor"
        "chasm"
        "climb"
        "stage"
        "quest"
        "world"
        "intelligence"
        "threat"
        "remote"
        "button"
        "lair"
        "vortex"
        "solitude"
        "window"
        "flash"
        "battle"
        "challenge"))
        

(define adjectives-new
  (list "hellish"
"exclusive"
"stale"
"light"
"decent"
"decorous"
"humorous"
"jaded"
"nostalgic"
"halting"
"brief"
"abiding"
"well-made"
"deep"
"marvelous"
"scattered"))

(define verbs-new
  (list "extract"
"imply"
"regain"
"practise"
"suck"
"cancel"
"compensate"
"market"
"express"
"perform"
"melt"
"replace"
"convict"
"in"
"wave"
"fight"))

(define nouns-new
  (list "tongue"
"foundation"
"attention"
"police"
"president"
"organization"
"map"
"knowledge"
"management"
"historian"
"ambition"
"employee"
"perspective"
"difficulty"
"secretary"
"cancer"))

(define nouns
  (list nouns-old nouns-new))

(define verbs
  (list verbs-old verbs-new))

(define adjectives
  (list adjectives-old adjectives-new))

(define all-categories
  (list nouns verbs adjectives))

(define random-list-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))

(define replace-all
  (lambda (old new lst)
    (if (null? lst)
        null
        (if (equal? (car lst) old)
            (cons new (replace-all old new (cdr lst)))
            (cons (car lst) (replace-all old new (cdr lst)))))))

(define replace-words
  (lambda (old-list new-list words)
    (if (null? old-list)
        words
        (replace-words (cdr old-list) new-list (replace-all (car old-list) (random-list-element new-list) words)))))


(define my-story
  (list "You materialize in the basement of Noyce. You feel like you’ve been saved from somehow falling into an inescapable void that would have consumed youand your soul. You see that all the doors around you are locked and the only way to escape is through the roof. You do not know what you’re doing there, why you’re there, who placed you here, or who you even are, but you begin at level 0: a dark, depressing dungeon known as the physics basement. It’s dangerous to go alone! Take this."
        "You see the floor beneath you shaking and now you are unable to continue forward. In order to keep going, you need to clear a [x] meter long gap [y] meters below you onto a slab that is 5 meters long. Your super (weapon) allows you to achieve any speed you want, the catch is that you can’t generate any downward force (you can’t jump). How fast do you run in order to land on the 5-meter-long slab?"
        "Congratulations, doofus! You’re either incredibly unathletic, unbelievably stupid, or just plain bad at physics. You must not have traveled at the correct speed to clear the gap, cause you just fell to your doom in the endless darkness below the surface of the earth! You’re dead!"
        "Not bad! You were either athletic enough, smart enough, or good enough at physics to clear the gap, because you figured out the correct speed to travel. As you cross the gap and continue to accelerate, you enter a strange warp tunnel with gigantic atoms flying past you. You dodge and weave through the storm of atoms flying at you, in the hope that you will not be harmed. Suddenly, you find yourself back in reality. You realize you’re on Level 1: the chemistry lab. You ALSO realize there are actual WINDOWS on this floor - giving you a glimpse into what awaits the end of your quest."
        "You notice several cracked containers of different elements scattered across the floor. As you lean down to inspect one, you realize it’s (random element) - which could infect everyone else working in the lab and kill you! Reasoning that all the other containers must contain similarly dangerous chemicals, you decide to help clear up the mess - but the only way to do so is to sort the scattered elements based on their category. Did you pay attention in high school chemistry class?"
        "Uh oh! You placed an element in a category that it doesn’t belong to. Triggered merely by your stupidity, one of the containers - a tanker of (the same random element) - suddenly explodes, quickly obliterating you, the entire floor and the rest of Noyce - but before the explosion can spread and consume the surrounding buildings, time stops. Suddenly, everything starts happening quickly in reverse - the explosion and your death are undone, but so is your trip through the warp tunnel, your clearing of the chasm in the basement, and the earthquake that created that chasm in the first place. You find yourself back in Level 0, exactly where you started. Some mysterious force just saved you from utter, explosive, (the same random element)-fueled oblivion, so be thankful!"
        "You managed to sort them all correctly. Maybe you do have potential! The strange, poisonous cloud that pervaded the floor fades. Nearby you notice a set of stairs and climb it. Surprisingly, the climb up is uneventful - which doesn’t add up, leaving you questioning whether the next stage in your quest will be only a fraction as dangerous as it was before - or exponentially more so. You now find yourself at Level 2: the math department."
        "You are now on the Math Floor. You don’t know why, but each floor looks completely different from the other. The Math Floor looks like a world from the future. Somehow, because of all the fast thinking and the automatic calculations that the inhabitants of that floor could do, the architecture of the place changed to a futuristic white spaceship. An old man in a technological suit says to you:“Hello, our work here is deciphering enigmas, problems that sometimes go hundreds and hundreds of years without being solved. We are detectives, and we have technology from the future. There’s one particular thing about our team: Everyone loves to play a Roulette bet machine. But it involves math. If you solve the following enigma, you might be able to pass it to our teleporter to the Third Floor because you prove yourself to be an enigma solver. In our roulette, there are (random number) numbers. And two colors. What is the probability (in a fraction) of guessing the right number, with the right color, (random number between 1 and 10) times in a row?"
        "Unlucky! The old man pulls out a remote and presses a button. In a flash, you find yourself back on the chemistry floor, with the elements scattered once again and the poisonous cloud once again pervading the air. Luckily you aren’t dead, but you’ve got to re-sort the elements once again if you want to succeed!"
        "Looks like you got lucky! The old man is impressed, and muses that with your good luck, an acceptable level of athleticism, and intelligence, you are indeed capable of defeating the threat upstairs. Confused, you ask the old man about this “threat” he speaks of. He tells you about the Great Rebelsky - an oppressive force that has enslaved everyone in the building and closed off the way out. He takes you to his teleporter - the only thing capable of transporting you to face the threat. He asks you, “Are you ready for the ultimate battle?” You nod, and step through the teleporter, warping up to the third and final floor - the dreaded Computer Lab."
        "You warp into the top floor - right into the lair of the Great Rebelsky! A swirling vortex of code and binary, the Great Rebelsky possesses the power to consume all it sees - and that includes you, now that you’ve disturbed its solitude! As the vortex slowly starts to increase in ferocity, you realize it’s moving slow enough for you to read some of the code that comprises it - and.... oh geez. That is horrible code. Suddenly, a gadget materializes in your hand, courtesy of the old man downstairs. The gadget will give you examples of code and allow you to upload one of them into the Great Rebelsky program. But you need to choose the best code among the given options in order to neutralize the virus and open up the way to the roof. Otherwise, there is no telling what will happen!"
        "You hit the upload button. Looks like you picked the bad code, because the swirling vortex is getting bigger, stronger, and seemingly angrier! It grows increasingly giant, consuming the entire building, and you along with it! But suddenly, a flash of light appears before your eyes, and you find yourself back on the math floor, with no memory of the great battle that just occurred. You see the old man once again and walk towards him, prepared to take on his roulette challenge."
        "You hit the upload button, and the swirling vortex of horrible code surely, but slowly dissipates. As a beam of light blazes through a window, you realize - you won! You step through the window and look out into the campus below you. But as grateful as you are to make it out of the building, you suddenly think to yourself, “How am I gonna get down from here?”"))

(define replace-words-category
  (lambda (category words)
    (replace-words (list-ref category 0) (list-ref category 1) words)))

(define replace-words-all-categories
  (lambda (categories words)
    (if (null? categories)
        words
        (replace-words-all-categories (cdr categories) (replace-words-category (car categories) words)))))

(define my-story-random-list-version
  (let ([x (map string-split my-story)])
    (map (section replace-words-all-categories all-categories <>) x)))

(define my-story-random
  (map (section string-join <>) my-story-random-list-version))
  






