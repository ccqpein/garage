---
title: ""
date: "2026-08-19"
slug: ""
tags: ["elisp", "llm", "antigravity"]
---

My friend recommend me the `Jujutsu` (jj) recently. And I learn how to use it with gemini. TBH, I recommend everyone learn how to use it with gemini since it is google's project. I feel like google's doc doesnt care how to use it, it only show off how good design it is. 

jj is the interesting tool, and I have been using it for weeks. I need a lot mind/behavior changing. Especially jj doesnt have the branch term, it actually change the whole logic of how to do the version control. And it is very hard to change my muscle memory, not only in terminal, but also in my emacs. 

In emacs, I use magit to do the git operation in emacs. Like after I make some changes of code, I check the diff, stage some of them, and commit. That's kind of the most frequent operations I need. Then I tried to find the same package of jj, I found the `jj-mode` but it cannot do the diff as magit does. 

# Let me vibe it

Ok, after all those background. I had an idea that I can vibe one. I actually did it with two major steps. With same AI but with different apps, design in Gemini, and implement in antigravity (agy).

## Design

The first step is I let the gemini know what I want to do. Especially the diff part. In magit, I pick the hunks of code and staged them. jj doesnt have the "stage", so I actually commit it directly. Then run the jj new to start a new commit (workplace). I didn't even know how pick the special hunk code to commit in jj at that time.

When Gemini teach me how to do partial commit, it tell me there is a command called `split`. In the design of this mode, gemini also mentioned this command. Gemini provided me three method, split is the most jj native way. So this mode need to let jj split gives the diff to it. Then update the env var by giving which code I choose. jj just handle everything else. 

The second thing jumped out was how to handle those diff code. jj split only receive the change (the code I pick), but wont do the diff render for me. Gemini also gives me some options after I denied its first answer. I let it use emacs self rather than write some python script. Because I dont want to involed more other dependencies.

It would be easy to write the script in python before LLM, but now we have those strong helper, write a elisp function for handling the diff isn't that hard. 

After all ideas check; decision made; purpose aligning, I let gemini write a whole requirement document for me. 

## Implement

After I get the requirement txt file, I let agy to write it. And I load it and use it to commit the code change of jj-diff repo itself. 

## Result

It done pretty well, even I updated some features change after, agy can finished it as my expect.

And this is the repo of [jj-diff](), my first 100% vibe repo (by now).

# Then, where is the debugging contest?

After I updated my emacs config in my work laptop with adding the `jj-diff`. Call the `jj-diff` will returns me some error. It is pretty weird that same config of emacs have different behaviors.

## How is agy doing

So I start the agy in my `jj-diff` folder on my personal laptop, I cannot use gemini in my work laptop so I can only tell agy what can I see. Agy first move is changing the code, with its own assumption. Which I didn't commit it, I just for some reason feel it isn't right. 

So I run severl more tests on my work laptop and personal laptop. Then tell agy what's the differences between. In my personal laptop, look like everything is fine, so I start to think if it is someting wrong with some share pkg? emacs version? (so I update my emacs) straight.el cache? Github problem (it had incent in that morning really).

I keep trying and sync what I get with agy. Agy still trying to change the code until I stop it. Then let me try some emacs function to get the information. TBH, the information it let me check aint matter at all. They are just 99% issues test function. But what I got it, is that 1%.

## Ah ha, I won

Then, I suddenly find the problem. I use the jj-mode in my laptop before for playing around. And I forgot to comment it, which I not even load it in my personal laptop. So the jj-mode's `jj-diff` function loaded. And it re-write the `jj-diff`'s `jj-diff` function. That's it. Delete the `jj-mode` and everything is fine now.

## Toughts

There are a lot conversations these years talk about what's value of human if AI will take everything. 

There are a lot answers from a lot where. I agree some of them. Beside I truly find some values in this debugging processing only for me. 

> 1. I have some ability of getting some context that AI cannot reach. 
> 2. I jump out some idea/spark that to check the config upper.

AI not even think about the possiblity that I screw up the config which is the problem can match all symtoms: 

+ Keybinding issue
+ Function error output
+ Back to work after I manully load the `jj-diff`.

Yes, because AI cannot read my laptop, that's maybe why AI keep repeating it self; Keep asking me to run the functions again and again that I already know the results; Keep trying to change the code those not nessasery at all. But, it is just approve my idea of the value of my role in this situation.

# Wrap up

I actually get two thoughts about this `jj-diff` writing and debugging journey.

+ I think I find a vibe coding workflow. Brainstorming ideas with AI's API/doc reading ability, then make it as the guideline/document, let agent write it.
+ Even in some area that everyone think AI gonna domminated, human's value might appear somewhere.
