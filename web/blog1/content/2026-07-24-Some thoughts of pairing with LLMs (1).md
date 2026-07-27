---
title: "Some thoughts on pairing with LLMs (1)"
date: "2026-07-24"
slug: "some-thoughts-of-pairing-with-llms-1"
tags: ["llm"]
---

Months ago, I wrote [an article](https://ccqpein.me/posts/some-thoughts-of-pairing-with-llms) about how I pair my daily tasks with LLMs.

Since then, LLMs have been evolving so fast. It feels like every few weeks, new models, new tools, new ideas, and new terms are released. I didn't use agents for a while, but then I tried `gemini-cli` (now `antigravity-cli`), and it was pretty helpful.

I still use Gemini's app, especially Notebook, as well as for many other questions, general knowledge, and learning.

As my workflow and tasks have expanded with agents, I have more thoughts about how to pair with LLMs. So, here we go.

# Context Switching

The first and hardest problem I encounter is context switching. Let me give an example: I want to implement a feature. First, I have a general idea, and I write some code to define the structure. Then, I run into some tasks that I want the LLM to write for me. What should I do? I switch to the terminal running the agent and let it code for me. Then I check if it matches what I had in mind.

Sounds easy, right? However, I found that most of the time, there are several tasks I want to implement concurrently. Furthermore, some of them are related. So I need to describe each of them to the agent, review and adjust the next task's requirements, and repeat these steps again.

This workflow requires a lot of context switching in my brain and pressure-tests my working memory. It slows down my coding and breaks my flow state.

## Wait a Moment

Suddenly, I remembered my first baby (or rather, my first personal tool), [code-it-later](https://github.com/ccqpein/code-it-later-rs). I wrote it to leave ideas (crumbs) and come back to them in the future. And who says the one coming back has to be me? Now, it can be the agent.

## Time to Learn Skills

I've always thought LLMs are the killer abstraction layer for API calls since ChatGPT 3.5. They filled the gap between natural language and APIs. So I was extremely happy when I could use function calling (if anyone still remembers this term) in the GPT API. But it wasn't good enough in my testing—it broke my heart. Then MCP was introduced. Then Agents with Skills. I knew about them, but I was a bit too lazy to get hands-on and try them out.

Now, with `antigravity-cli` and a "let the agent learn how to use `code-it-later`" request, it was the perfect time to learn Skills. I asked Gemini some questions about Skills and had Gemini write the skill for me. After several rounds of improvements, my `antigravity-cli` learned the [code-it-later skill](https://github.com/ccqpein/code-it-later-rs/blob/master/SKILL.md). And it is pretty easy to [use](https://github.com/ccqpein/code-it-later-rs#google-antigravity-integration).

## Code-it-later Strikes Again!

Now, I can stay in my coding flow. When I encounter features that I can leave unimplemented, I just leave crumbs behind. Then, while I take a restroom break, grab coffee, or switch to working on other files, I can let the agent pick up those crumbs and implement them. I love it.

# Lazy LLMs

Here is another observation I've had recently:

> LLMs have a motivation to be lazy.

LLMs generally want to save tokens because (in my assumption):

+ Saving context
+ Saving time

Just like human beings. I often use this example: if there is a function in Module B where adding one argument and five lines of code would solve the goal in Module A, an LLM will often try to write a completely new function in Module A instead.

For human programmers, the better solution is reusing the function in Module B (not 100% of the time, of course) and keeping the codebase clean and maintainable. But for an LLM, doing that requires reading a lot of files and trying to understand the whole project structure. It definitely can do it if I explicitly ask, but by default, it tries its best to avoid it.

## Controlling the Agent's Scope

To address the laziness of LLMs, I first need to thoroughly understand my codebase (at least its file structure) and keep it well-organized. Then, I need to make sure the agent **only** modifies the specific files or scope I specify (or just use `code-it-later`!).

## Providing Explicit Assumptions

If there are two ways to reach a goal, an LLM will make some hidden assumptions to take the easiest path. But some of those assumptions will affect my code in the future. In such cases, I need to align assumptions either before or while it is doing the job.

# Wrap-Up

I'm pretty sure there will be a lot of new LLM tools and features released in the future. I'm always open to trying them out, and I will definitely do so. I will keep sharing my experiences and thoughts on pairing with LLMs in future articles.
