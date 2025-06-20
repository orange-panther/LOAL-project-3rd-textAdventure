/* phillippines adventure by Flora Dallinger, Lena Grassauer and Katharina Einzenberger */

:- dynamic i_am_at/1, at/2, holding/1, recipe/2, inside/2, cupboard_open/1, close_object/1, accepted/1, completed/1, path/3.
:- discontiguous open_object/1, yes/0.

introduction :- 
        write('The sea lies calm. Tropical heat shimmers over the wooden deck of your ship as the sun goes down. Gulls circle above the coast—white sand, dense jungle, smoke rising from the huts on the horizon. After months at sea… finally land.'), nl,
        write('The Philippines. A place not yet marked on your map. A distant edge of the world—and perhaps the end of your journey.'), nl,
        write('You are Ferdinand Magellan, captain of the last remaining ships of your fleet. On behalf of the Spanish crown, you have crossed the West—through storm, hunger, and mutiny. And now, you are here. Standing beside your loyal companion Uwentus.'), nl.

/* This rule just writes out game instructions. */ 

/* TODO: rewrite to actual commands*/
instructions :- 
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                  -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.  -- to go in that direction (north, south, east, west, up, down).'), nl,
        write('take(Object).           -- to pick up an object.'), nl,
        write('take_out(Object).       -- to take an object out of a drawer.'), nl,
        write('drop(Object).           -- to put down an object.'), nl,
        write('look.                   -- to look around you again.'), nl,
        write('open_object(Drawer).    -- to open a drawer or cupboard.'), nl,
        write('close_object(Drawer).   -- to close a drawer or cupboard; IMPORTANT: please remember to always close a drawer, or you will not be able to open it again.'), nl,
        write('recipes.                -- to show all recipes you are able to craft.'), nl,
        write('talk(Villager).         -- to talk to someone.'),nl,
        write('craft(Object).          -- to craft an object that you looked up in the recipe book; you have to hold the needed items.'), nl,
        write('instructions.           -- to see this message again.'), nl,
        write('sail_away.              -- to end the game and leave the island.'), nl.

/* start */
start :- 
        retractall(i_am_at(_)),
        assert(i_am_at(boat_deck)),
        retractall(holding(_)),
        retractall(accepted(_)),
        retractall(completed(_)),
        introduction, 
        instructions,
        nl,
        look.
        

/* These facts tell where the various objects in the game
   are located. */

at(wood, pantry).
at(barrels, pantry).
at(cupboard, captains_cabin_cupboard).
at(flint, cupboard).
at(stone, river).

/* describe what objects are inside something */

inside(flint, cupboard).

/* rule to define what can be opened */
can_be_opened(cupboard).
cupboard_open(false).

/* rule to open something */
open_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        can_be_opened(Object),
        cupboard_open(false),
        retract(cupboard_open(false)),
        assert(cupboard_open(true)),

        % Finde alle Items, die sich aktuell im Objekt befinden
        findall(Item, inside(Item, Object), Items),

        % Inhalt anzeigen oder sagen, dass es leer ist
        ( Items = [] ->
                write('It is empty.'), nl
        ;
                write('You open the '), write(Object),
                write(' and find: '), write(Items), nl
        ),
        !.

open_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        (\+ can_be_opened(Object); cupboard_open(true)),
        write(Object), write(' cannot be opened.'), nl,
        !.

open_object(_) :-
        write('There is nothing like that to open here.'), nl,
        !.


/* rule to close something */
close_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        can_be_opened(Object),
        cupboard_open(true),
        retract(cupboard_open(true)),
        assert(cupboard_open(false)),
        !.

close_object(Object) :-
        i_am_at(Location),
        at(Object, Location),
        \+ can_be_opened(Object),
        write(Object), write(' cannot be closed.'), nl,
        !.

close_object(_) :-
        write('There is nothing like that to close here.'), nl,
        !.

/* describe how the locations are connected */

path(boat_deck, e, beach).
path(beach, w, boat_deck).

path(boat_deck, w, captains_cabin).
path(captains_cabin, e, boat_deck).

path(captains_cabin, n, captains_cabin_cupboard).
path(captains_cabin_cupboard, s, captains_cabin).

path(boat_deck, d, pantry).
path(pantry, u, boat_deck).

path(beach, s, river).
path(river, n, beach).

path(beach, e, jungle).
path(jungle, w, beach).

path(jungle, e, village).
path(village, w, jungle).

path(village, s, rajah_hut).
path(rajah_hut, n, village).

path(village, n, village_district).
path(village_district, s, village).

path(village_district, w, philipom_house).
path(philipom_house, e, village_district).

path(village_district, e, marki_house).
path(marki_house, w, village_district).

path(marki_house, e, markis_field).
path(markis_field, w, marki_house).

path(village_district, n, village_district_end).
path(village_district_end, s, village_district).

path(village_district_end, e, antoninon_house).
path(antoninon_house, w, village_district_end).

path(village_district_end, w, llorena).
path(llorena, e, village_district_end).

/* These facts tell you if villagers exist and where they are located*/
villager(philipom).
villager(marki).
villager(antoninon).
villager(luena).
villager(llorena).
villager(pagipogi).
villager(rajah).

villager_located(philipom, philipom_house).
villager_located(marki, marki_house).
villager_located(antoninon, antoninon_house).
villager_located(rajah, rajah_hut).
villager_located(llorena, llorena_house)

/* These facts tell you how to talk to someone*/
talk(Villager) :-
    villager(Villager),
    villager_located(Villager, philipom_house),
    i_am_at(philipom_house),
    holding(axe),
    accepted(philipom_task),
    write('Philipom looks at your hand. philipom: Is this axe for me? [yes. | e.]'), nl,
    !.
    

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, philipom_house),
    i_am_at(philipom_house),
    \+accepted(philipom_task),
    write("philipom: Hello... I am Philipom. I need some wood for my fireplace... but my axe broke before I could get any. Rajah will be mad at me if I won't."), nl,
    write("philipom: All i need is an axe, please help me."),
    write("Would you help him? [yes.]"),nl,
    !.

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, philipom_house),
    i_am_at(philipom_house),
    accepted(philipom_task),
    \+holding(axe),
    write('philipom: Have you got my axe yet? ... Please hurry'), nl,
    !.


talk(Villager) :-
    villager(Villager),
    villager_located(Villager, marki_house),
    i_am_at(marki_house),
    accepted(marki_task),
    \+holding(grain),
    write('marki: Have you got the food yet? ... Please hurry'), nl,
    !.

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, marki_house),
    i_am_at(marki_house),
    holding(grain),
    accepted(marki_task),
    write('marki: Is this food for us?... [yes. | go(_).]'), nl,
    !.

    talk(Villager) :-
    villager(Villager),
    villager_located(Villager, marki_house),
    i_am_at(marki_house),
    write("marki: Hello stranger, I am Marki. "),
    write("I need your help, my family is starving ... We have a field but no hoe. I need to save my wife and kids :("), nl,
    write("Would you help him? [yes.]"),
    !.

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, antoninon_house),
    i_am_at(antoninon_house),
    accepted(marki_task),
    write("You: Hello, my name is Ferdinand. I am here to help some villagers and I am currently searchin for a hoe. Do you have one I might borrow?"), nl,
    write("Antoninon: A hoe you say? Of course, but I don't give them to strangers! Well maybe the old one, here so called Ferdinand [yes.] to accept hoe"),
    !.


talk(Villager) :- 
    villager(Villager),
    villager_located(Villager, rajah_hut),
    i_am_at(rajah_hut),
    accepted(rajah_main_task),
    completed(marki_task),
    completed(philipom_task),
    ( accepted(rajah_main_task) ->
        retract(accepted(rajah_main_task)),
        assert(completed(rajah_main_task)), 
        write('Task completed: rajah_main_task'), nl
    ; 
        true
    ),
    write('Rajah: Ferdinand, you are back and you have gained my trust. Now i need to tell you something...
                Long ago, me and Lapu Lapu were allies, but then he turned against us. For years he has been threatening us that he will destroy the fields or houses.
                Now I have decided, it''s enough. In a short time there will be war...
                Ferdinand, I need to ask you if you fight on our side? [yes.] [n.]'),
    assert(path(village, e, tidal_strait)), /* tidal strait ist die Landbrücke zwischen den 2 Inseln */
    !.

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, rajah_hut),
    i_am_at(rajah_hut),
    accepted(rajah_main_task),
    write('Rajah: You haven\'t gained enough status yet.'), nl,
    !.

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, rajah_hut),
    i_am_at(rajah_hut),
    write('You: I am Ferdinand Magellan, I come not to conquer, but to befriend the distant lands of this world. 
    A long silence. Then, his answer—measured, sharp:
    Rajah: If you truly seek friendship, you must first prove your loyalty. [yes.]|[no.]'), nl,
    !.

talk(Villager) :-
    villager(Villager),
    villager_located(Villager, llorena_house),
    i_am_at(llorena_house),
    write('You approach the women. The moment she hears your footsteps she looks up at you holding her watering can in front of her as if she wants to protect herself with it.'), nl,
    write('You: I come in peace'), nl,
    write('You put up your hands in a sign of surrender'), nl,
    write('Llorena: I shouldn\'t talk to strangers. Especially not god denying ones'), nl,
    write('She looks at the cross necklace around your neck'), nl,
    write('Should you try and convince her of your religion? [yes.]|[e.]'), nl,
    !.

/* These facts tell what you can craft out of objects */

recipes :- 
    recipe(New, X, Y),
    write('To craft '), write(New),
    write(', you need '), write(X),
    write(' and '), write(Y), nl,
    fail.
recipes.  % succeeds after all options fail

recipe(torch, flint, wood).
recipe(axe, stone, wood).

/* This rule tells how to look around you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        list_inventory,
        list_tasks,
        !.

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.

/* These rules describe how to take an object out of a drawer */

take_out(X) :-
         holding(X),
        write('You''re already holding it!'),
        !, nl.

take_out(X) :-
        i_am_at(captains_cabin_cupboard),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take_out(_) :-
        write('I don''t see it here.'),
        nl.

/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'), nl,
        !.

drop(_) :-
        write('You aren''t holding it!'),
        nl,
        !.

/* These rules define the six direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

u :- go(u).

d :- go(d).

/* This rule is how you move */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        !,
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look,
        nl.

go(_) :-
        write('You can\'t go that way.').



/* These rules craft an object */
craft(New) :- 
        recipe(New, X, Y),
        holding(X),
        holding(Y),
        retract(holding(X)),
        retract(holding(Y)),
        assert(holding(New)),
        write('Holding new object '), write(New),
        !, nl.

craft(_) :- 
        write('You cannot craft such an object or don\'t have the ressources for it'), nl.


/* This rule lists all of your elements */
list_inventory :-
        findall(Item, holding(Item), Items),
        ( Items = [] ->
            write('You are not holding anything.'), nl
        ; 
            write('You are holding: '), write(Items), nl
        ).


/* This rule lists all of your tasks */
list_tasks :- 
        findall(Task, accepted(Task), Tasks),
        ( Tasks = [] -> 
            write('You have no active tasks'), nl
        ;
            write('You are tasked: '), write(Tasks), nl
        ).

/* These rules are for choices */
yes :- i_am_at(jungle),
        holding(torch),
        write('You have entered the jungle. You hear some noises in the bushes while you walk through it, but the light from you torch is keeping the animals away.'), nl,
        go(e), 
        !.

yes :- i_am_at(jungle),
        \+holding(torch),
        die. 

yes :- i_am_at(river), 
        die.

yes :-
        i_am_at(tidal_strait),
        die.

no :- i_am_at(rajah_hut),
        die.


yes :- i_am_at(rajah_hut),
        write('You have to help some villagers in order to continue the story'), nl,
        assert(accepted(rajah_main_task)),
        list_tasks,
        !.

yes :- i_am_at(llorena_house),
        write('You begin to talk about your religion: your god, the benefits and the rules for what seems to be hours.
                After you finished your lecture Llorena looks at you and gives you an akward smile before she runs back inside her house.'), nl,
        !.
        

yes :-
        i_am_at(marki_house),
        accepted(marki_task),
        holding(grain),
        retract(holding(grain)),
        ( accepted(marki_task) ->
                retract(accepted(marki_task)),
                assert(completed(marki_task)), 
                write('Task completed: marki_task'), nl
        ; 
                true
        ),
        !.

yes :- 
        i_am_at(philipom_house),
        accepted(philipom_task),
        holding(axe),
        retract(holding(axe)),
        ( accepted(philipom_task) ->
                retract(accepted(philipom_task)),
                assert(completed(philipom_task)),
                write('Task completed: philipom_task'), nl
        ; 
                true
        ),
        !.

yes :- i_am_at(philipom_house),
        write('What? ... You want to help me? Thank you traveler.'), nl,
        \+accepted(philipom_task),
        assert(accepted(philipom_task)),
        !.
yes :- i_am_at(marki_house),
        write('You are gonna help me?.. You are saving us, thank you'), nl,
        \+accepted(marki_task),
        assert(accepted(marki_task)),
        !.

yes :- 
        i_am_at(antoninon_house),
        accepted(marki_task),
        assert(holding(hoe)),
        write('You are now holding a hoe.'), nl,
        !.



/* rules to plow the field */
plow :-
        i_am_at(markis_field),
        holding(hoe),
        assert(holding(grain)),
        write('You plow the field. After you are finished you pick up the grain'), nl,
        list_inventory.

/* These rules describe the various rooms. */
describe(boat_deck) :- write('You are currently on the deck of the last remaining boat of your fleet. You have a broad deck, two stairs and the entry to the captains cabin.'), nl.
describe(captains_cabin) :- write('You now have entered the cabin of the captain... you.'), nl.
describe(pantry) :- write('You are currently in the pantry where food and drinks are stored.'), nl.
describe(beach) :- write('You are standing on the beach after you left the boat. You hear seaguls screeching and the waves crushing in. On the north you see the beginning of a gigantic wood. You can hear the monkey  Uwentus is right behind you.'), nl.
describe(captains_cabin_cupboard) :- write('You are standing in front of a cupboard in your cabin.'), nl.
describe(jungle) :- write('You are standing at the entrance of a jungle. It\'s looking grim and dangerous.'), nl.
describe(river) :- write('You reach a river, its surface black and still. You can just make out a line of stones crossing to the other side.'), nl.
describe(village) :- write('You come out of the jungle and see a little village. You and your crew enter it.'), nl.
describe(rajah_hut) :- write('The largest house rises in its center, tall and still. Two guards watch you approach but let you pass without a word. You turn to your crew. “Stay here,” you command. They obey. Inside, the air is dense and warm. At the far end, seated like a statue carved from wood and pride, is Rajah Humabon. He watches your every step. You stand tall, despite the weight of the journey.'), nl.
describe(village_district) :- write('As you walk deeper in the village, you notice small huts around you.'), nl.
describe(philipom_house) :- write('In the distant you see a sad man. You and Uwe walk up to the house. The man stands outside of it [his name is philipom]'), nl.
describe(marki_house) :- write('You see a small house, some ill looking children are laying in front of it. Then a skinny man comes out. [his name is Marki] '), nl.
describe(village_district_end) :- write('The path narrows and the last of the huts give way to thick jungle once more.'), nl.
describe(antoninon_house) :- write('You see a big house with lots of tools and weapons in front of it, in the door frame an intimidating man. [his name is Antoninon]'), nl.
describe(markis_field) :- write('You see a small uneven field, carrot and potato tops spoke out of the dirt and wheat plants swaying gently in the breeze.'), nl.
describe(tidal_strait) :- write('A long wooden bridge stretches between the two islands, beneath it the dark ocean.'), nl.
describe(llorena_house) :- write('You walk up to another house. Seemingly smaller than the other ones.'), nl.

/* rules for des:cribing which objects are around player */

notice_objects_at(boat_deck) :- 
        write('You find nothing around you except the wide sea and your faithful friend Uwentus'),nl.
notice_objects_at(captains_cabin) :- 
        write('It\'s dark and stuffy in here. Around you there are some cupboards and drawers.'),nl.
notice_objects_at(pantry) :- 
        write('You find some dusty barrels lying around. Behind them you can see some wood.'), nl.
notice_objects_at(beach) :- 
        write('The beach is relatively empty. Some stones and shells lying around... nothing special'), nl.
notice_objects_at(captains_cabin_cupboard) :- 
        write('The cupboard is closed. You have to open it before you can access anything inside.'), nl.
notice_objects_at(jungle) :- 
        write('You can make out some trees and bushes in the darkness, but it is all overgrown and you wouldn\'t even dare to leave the path. You have the choice to enter the jungle [yes.] or go back to the beach [w.]'), nl.
notice_objects_at(river) :- 
        write('You see no other living being around you, you only hear the sound of the water running downstream. But there are some stones laying around. You have the choice to cross the river [yes.] or go back to the beach [n.]'), nl.
notice_objects_at(village) :- 
        write('You see some people by their huts. They look at you curiously, as if they were trying to determine whether you are a friend or a foe. Maybe you could try to approach one of them.'), nl.
notice_objects_at(village_district) :- 
        write('You notice some huts on the left and right of you.'), nl.
notice_objects_at(village_district_end) :- 
        write('You go on a path trough the last huts, but you can''t go further...
                You need to go back [s|w|e]'), nl.
notice_objects_at(philipom_house) :- 
    write('Philipom stands silently outside his hut, his eyes heavy with worry. Beside him, a cracked water jug rests on the ground, and the scent of bitter herbs floats in the air.'), nl.
notice_objects_at(marki_house) :- 
    write('Marki, thin and pale, kneels beside the sick children, whispering quietly to one of them. Small cloth bundles and simple toys lie scattered nearby, mixing with the faint smell of smoke and old medicine.'), nl.
notice_objects_at(village_district_end) :- 
    write('No one is here. The last huts fade into the jungle, and only the rustle of leaves and distant birds break the stillness. Some faint footprints lead into the trees.'), nl.
notice_objects_at(antoninon_house) :- 
    write('Antoninon stands in the doorway, arms crossed, watching you with a stern gaze. Around him lie sharpened tools, stacked wooden crates, and a heavy chest sealed with iron.'), nl.
notice_objects_at(markis_field) :- 
    write('You see rows of carrots, potatoes and wheat growing in the soil. A simple wooden fence marks the edge of the field.'), nl.
notice_objects_at(rajah_hut) :- 
    write('The Rajah\'s hut stands taller than the others, decorated with bright cloth and carved wood. Two guards watch silently near the entrance.'), nl.
notice_objects_at(tidal_strait) :- 
    write('Below the bridge, the tidal strait shimmers with moving water. Fish dart through the current, and you can hear the steady rhythm of waves against stone.'), nl.
notice_objects_at(llorena_house) :- 
    write('In the garden you notice a woman watering the flowers. [Her name is Llorena]')

/* rules for describing the death of the player */

die :- i_am_at(jungle),
        write('The jungle echoed with the sounds of the night—chirping insects, distant howls, rustling leaves. 
You can barely see, each step is cautious and slow. 
Suddenly, a low growl cut through the silence. Out of the shadows, two glowing eyes appeare. Before you can run, the tiger lunged. There was no escape. 
The last thing you hear was the roar. Then—darkness. 
Maybe it would be a good idea to enter the jungle with some light source next time.'), nl, nl,
halt.

die :- i_am_at(river),
        write('Carefully, you step onto the first one—cold, slick, but steady. One step, then another.
Then the "stone" moved.
A pair of eyes opened beneath you. Teeth flashed. Too late, you realize—they aren’t stones. They are alligators.
The river came alive.
And you became the opposite...'), nl, nl,
halt.

die :- i_am_at(rajah_hut),
        write('Rajah is not happy that you said "No", he tooks out an axe and slams it in your scull. Your crew also gets killed by the guards'), nl, nl,
halt.

/* TODO: insert ending */

die :- i_am_at(tidal_strait),
        write('
        At dawn, you set out with Rajah Humabon and his warriors. The boats glide silently over the calm sea. Your crew prepares their muskets, tense and quiet. Ahead lies Mactan—Lapu-Lapu’s land.
        You reach the shore. The enemy is already waiting.
        You give the signal. Muskets fire. Smoke. Screams. A few of them fall—but most charge without fear.
        Their spears come fast. Your powder is damp. Reloading is too slow.
        They reach your line.
        You swing your sword, but it’s not enough. The tide pulls at your legs. Your men are falling. Rajah’s warriors break and scatter.
        Their spears fly faster than you can raise your arm. One struck your leg. You stumble in the surf. Another hit your side. The water is turning red.
        You look up Lapu-Lapu’s men close in.
        The last thing you see is the rising sun.
        And then... nothing. '), nl, nl,
halt.

/* rules for the win of the player */

sail_away :-
        i_am_at(boat_deck),
        write('Congratulations! You did the only right thing to do in this situation. You sailed away and left the people of the foreign island live their life in peace.'),nl,
        halt.
sail_away :-
        write('You have to be on the boat deck to sail away.'), nl.        