---
title: "Some thoughts of pairing with LLMs (2) - I won LLM on the debugging"
date: "2026-08-21"
slug: "some-thoughts-of-pairing-with-llms-2"
tags: ["elisp", "llm", "antigravity"]
---

My friend recommended `Jujutsu` (jj) to me recently. And I learned how to use it with Gemini. TBH, I recommend everyone learn how to use it with Gemini since it is Google's project. I feel like Google's doc doesn't care about how to use it; it only shows off how good the design is. 

jj is an interesting tool, and I have been using it for weeks. It took a lot of mindset/behavior changes. Especially since jj doesn't have the concept of branches, it actually changes the whole logic of how to do version control. And it was very hard to change my muscle memory, not only in the terminal, but also in Emacs. 

In Emacs, I use Magit to do Git operations. Like, after I make some code changes, I check the diff, stage some of them, and commit. That's kind of the most frequent operation I need. Then I tried to find an equivalent package for jj. I found `jj-mode`, but it cannot do diffs like Magit does. 

# Let me vibe it

Ok, with all that background, I had an idea that I could vibe one. I actually did it in two major steps, using the same AI but across different apps: design in Gemini, and implement in Antigravity (agy).

## Design

The first step was letting Gemini know what I wanted to do, especially the diff part. In Magit, I pick hunks of code and stage them. jj doesn't have "staging", so I actually commit directly, then run `jj new` to start a new commit (working copy). I didn't even know how to pick specific code hunks to commit in jj at that time.

When Gemini taught me how to do partial commits, it told me there is a command called `split`. In the design of this mode, Gemini also mentioned this command. Gemini provided me with three methods; `split` was the most jj-native way. So this mode needs to let `jj split` provide the diff to it, then update the env var with the code I choose. jj just handles everything else. 

The second question that popped up was how to handle the diff code. `jj split` only receives the changes (the code I pick), but won't render the diff for me. Gemini gave me some options after I rejected its first suggestion. I had it use Emacs itself rather than writing a Python script, because I didn't want to involve more dependencies.

It would have been easier to write the script in Python before LLMs, but now that we have these strong helpers, writing an Elisp function to handle the diff isn't that hard. 

After checking all the ideas, making decisions, and aligning the scope, I had Gemini write a full requirements document for me. 

## Implement

After I got the requirements txt file, I let agy write it. Then I loaded it and used it to commit code changes in the `jj-diff` repo itself. 

## Result

It turned out pretty well. Even when I added some feature changes later, agy finished them as expected.

And this is the repo for [jj-diff](https://github.com/ccqpein/jj-diff.el), my first 100% vibe repo (so far).

# Then, where is the debugging contest?

After I updated my Emacs config on my work laptop by adding `jj-diff`, calling `jj-diff` returned an error. It was pretty weird that the same Emacs config had different behaviors.

## How agy did

So I started agy in my `jj-diff` folder on my personal laptop. I couldn't use Gemini on my work laptop, so I could only tell agy what I saw. agy's first move was changing the code based on its own assumptions. I didn't commit that—for some reason, I just felt it wasn't right. 

So I ran several more tests on my work laptop and personal laptop, then told agy what the differences were. On my personal laptop, everything looked fine, so I started wondering if something was wrong with a shared package? Emacs version? (so I updated my Emacs) `straight.el` cache? GitHub incident? (there really was an incident that morning).

I kept trying things and syncing what I found with agy. agy kept trying to change the code until I stopped it. Then it had me try some Emacs functions to get information. TBH, the information it asked me to check didn't matter at all. Those were just test functions for the 99% of common issues. But what I got was that 1%.

## Aha, I won

Then, I suddenly found the problem. I had used `jj-mode` on my work laptop before to play around, and I forgot to comment it out (which I hadn't even loaded on my personal laptop). So `jj-mode`'s `jj-diff` function was loaded, and it overwrote my `jj-diff` package's `jj-diff` function. That was it. I deleted `jj-mode`, and everything is fine now.

## Thoughts

There have been a lot of conversations in recent years about what the value of humans is if AI takes over everything. 

There are a lot of answers from all over the place. I agree with some of them. Besides, I truly found some value in this debugging process that was unique to me:

> 1. I have the ability to get context that AI cannot reach. 
> 2. I had a sudden spark/idea to check the config higher up.

AI didn't even think about the possibility that I screwed up the config, which was the only root cause that matched all the symptoms: 

+ Keybinding issue
+ Function error output
+ Back to working after I manually loaded `jj-diff`.

Yes, because AI cannot read my laptop, that's probably why AI kept repeating itself; kept asking me to run functions again and again whose results I already knew; kept trying to change code that wasn't necessary at all. But it just proved my point about the value of my role in this situation.

# Wrap up

I actually have two takeaways from this `jj-diff` writing and debugging journey:

+ I think I found a vibe coding workflow: brainstorm ideas with AI, because its API/doc reading ability, make it into a guideline/document, and let the agent write it.
+ Even in areas where everyone thinks AI is going to dominate, human value can still show up somewhere.
