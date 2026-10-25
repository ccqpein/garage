---
title: "Rust Optimization Techniques in cl-format-rs"
date: "2026-10-24"
slug: "rust-optimization-techniques-in-cl-format-rs"
tags: ["rust", "lisp", "performance"]
---

`cl-format-rs` is a fun project I made before the AI era. TL;DR: it lets you run Common Lisp's `format` function in Rust. 

I wrote a [blog post](https://ccqpein.me/posts/make-common-lisp-format-function-into-rust) recording how I made it. At the end of the article, I was trying to optimize it. A few days ago, I suddenly remembered it and decided to let AI optimize it for me. It went pretty well, and I truly learned something. 

So I'm writing this down as notes on optimizing a Rust project. 

# Allocation optimization

Two years ago, I generated some flamegraphs and they showed that most of the time was spent on string allocation. Which makes total sense, because this library is essentially a string builder. So allocation issues were my first target. I had Antigravity inspect the codebase focused solely on allocation issues (controlling the LLM's workflow is how I use it).

I also reviewed all the code it changed. Some of the changes were pretty interesting and ideas I had never thought of. 

## Clone Clone Clone

Using `.clone()` was the trick I relied on when I learned Rust back in 2018. Now I don't really need that trick anymore. So the first step was simply to delete all `.clone()` calls.

## Temporary allocations

Temporary allocations are a big deal too. Even though temporary variables drop safely in Rust, creating them repeatedly still costs a lot of time. 

For example, in `buf.push_str(self.to_string().as_str())`, `to_string()` allocates a temporary `String` on the heap. I originally wrote this just to keep types simple, but in reality, different types need to be handled differently:

+ Strings can just use `push_str`.
+ `char` (and similar primitives) can just use `buf.push(*self)` since primitive types implement `Copy`.
+ Other types can use `write!(buf, "{}", self)`.

Furthermore, `String::from_utf8(vec![b'~'; *n])` can be replaced with `buf.extend(std::iter::repeat('~').take(*n))`. This saves both a `vec![]` and a `String` allocation.

## Pre-allocate String Capacity

I know about `with_capacity` and use it often, but I used to pass the wrong length when calling it. Strings grow their capacity dynamically (e.g., 1 -> 2 -> 4...), so by providing an estimated correct length upfront, I saved some allocation time.

## Just Cell

`RefCell` is an old friend. We go way back because of all the linked lists I implemented before (`Rc<RefCell<T>>`). But this time, I needed to change it to `Cell`. The `RefCell<usize>` I used was unnecessary: `usize` implements `Copy`, and all the runtime borrow-tracking overhead of `RefCell` is pointless for a `usize` in this case.

## Cow Mooooo

I used `Cow` [before](https://ccqpein.me/posts/why-the-vec-grid-of-my-tool-is-that-slow-in-rust#what-do-the-test-codes-say), but I didn't think about using it in `cl-format-rs`, or maybe I just didn't know enough at the time. Either way, `Cow` is great for handling borrowed slices/vectors and giving them a "lazy" clone.

## Lazy is good

Replacing eager `.ok_or(TildeError::new(...))` with lazy `.ok_or_else(|| ...)`. This is because `Option::ok_or(err)` evaluates its argument **eagerly** before checking. Switching to `ok_or_else` saved a lot of time.

## Don't be too generic

I used `a as i64` a lot before writing or formatting numbers to strings. It made things simple, but casting `i32` to `i64` costs more memory and adds an extra conversion operation. Giving each type its own formatting function made things faster.

## Take it, just take it if you need

In some places, `cache.clone()` was changed to `std::mem::take(&mut cache)`. The cache holds a deep AST, and it was being cleared right after the clone anyway. Taking ownership of it makes a lot of sense.

## I write too recursively, I guess

In other languages, I like writing recursive functions like: 

```lisp
(defun read-until (bytes flag buf)
  (if (eq (first bytes) flag)
      (return buf)
      (read-until (cdr bytes) flag (append buf (list (first bytes))))))
```

Very functional, right? In `cl-format-rs`, I heavily used `Cursor::read_until`, and then sought back one position with `c.seek(SeekFrom::Current(-1))` right after. Furthermore, all I actually needed was the position index, so this incurred a lot of unnecessary I/O operations.

So `read_until` can be replaced with an iterator's `position`:

```rust
while let Some(rel_pos) = bytes[has_read_len..].iter().position(|&b| b == b'~') {...}
```

# Architecture optimization

After *Allocation optimization*, I asked Gemini for ideas on how to optimize other parts of the project—like overall architecture and design features that could make `cl-format-rs` faster. 

It changed some things I had considered before but was too lazy to implement, such as adding broader type support. I will skip that type magic for now and focus on performance here. And TBH, some type changes didn't affect performance at all; the Rust compiler is already smart enough that those changes made no real difference.

## Zero-Copy, just borrow it

I generally avoid adding lifetimes to struct definitions because it often forces me to deal with lifetime annotations down the road. But sometimes putting references inside a struct isn't that bad, especially when values don't change often. And of course, we also have `Cow`.

## What is `:literal`?

This one is pretty interesting. I've written macros before, but I had never used `:literal`. However, `:literal` isn't the magic part. The magic part is using a static `OnceLock` inside the macro.

The new `cl_format!` macro looks like this: 

```rust
  ($control_str:literal, $($ele:expr),*) => {
      {
          static __CL_FORMAT_CS: std::sync::OnceLock<cl_format::ControlStr<'static>> =
              std::sync::OnceLock::new();
          let c = __CL_FORMAT_CS.get_or_init(|| {
              cl_format::ControlStr::new($control_str).expect("making control string has issue")
          });
          let arr = [$(tilde!($ele)),*];
          let a = cl_format::Args::new_cow(&arr);
          c.reveal(a)
      }
  };
```

It creates a static `ControlStr` and initializes it via `get_or_init` on the first call. Every subsequent call simply gets the value from the `OnceLock`. This saves a lot of time in the most common use case of `cl-format-rs`: calling it with a string literal like `cl_format!("User: ~a", user)`. This made the macro benchmark 5.1x faster on top of all the previous optimizations—a total speedup from `1842ns` down to `72.3ns` (about 25x faster).

# Type system optimization

Now for the last few things. They didn't make a big difference in performance, but they are still fun to look at.

## Tuples

I know some Rust libraries use macros to generate tuple implementations for traits, like [this one](https://docs.rs/actix-web/latest/actix_web/trait.FromRequest.html#foreign-impls). Gemini did the exact same thing for `cl-format-rs`.

It first changed the trait by removing the associated type `type Item: TildeAble + ?Sized;` and adding a bunch of methods:

```rust
pub trait ArgCursor {
        fn skip(&self) -> bool;
        fn back(&self) -> bool;
        fn left_count(&self) -> usize;

        fn pop_char(&self, tkind: &TildeKind, buf: &mut String) -> Result<()>;
        fn pop_float(&self, tkind: &TildeKind, buf: &mut String) -> Result<()>;
        fn pop_digit(&self, tkind: &TildeKind, buf: &mut String) -> Result<()>;
        ...
}
```

Then it used a macro to implement this trait for a series of tuples: 

```rust
pub struct TupleArgs<T> {
        tuple: T,
        ind: Cell<usize>,
    }

macro_rules! impl_tuple_arg_cursor {
    ($len:expr; $(($idx:tt, $T:ident)),+) => {
        impl<$($T: TildeAble),+> ArgCursor for TupleArgs<($($T,)+)> {
            #[inline]
            fn pop_va(&self, tkind: &TildeKind, buf: &mut String) -> Result<()> {
                let ind = self.ind.get();
                match ind {
                    $(
                        $idx => {
                            self.ind.set(ind + 1);
                            // Static dispatch! Each tuple field has its concrete type ($T):
                            self.tuple.$idx.format_va(tkind, buf)
                        }
                    )+
                    _ => Err(TildeError::new(ErrorKind::RevealError, "cannot reveal to Va").into()),
                }
            }
            // ... repeated for pop_digit, pop_char, etc.
        }
    }
}
```

I had originally used `type Item: TildeAble + ?Sized;` to handle generic types. But Gemini used tuples instead, which natively support heterogeneous types in Rust. I think Gemini's approach is much more idiomatic Rust. Removing `dyn` also made it slightly faster.

# Wrap up

That was a lot of optimizations, with plenty of details that weren't even related to each other. That's why this post ended up being a collection of notes. But I like it—I love seeing how tweaking a few specific details can lead to huge improvements.

Besides that, here are a few more thoughts from this optimization pass that I want to remember. Why? Even though we can now just let an LLM optimize our code after we write it, keeping these core principles in mind while writing code helps us make better decisions from the start. Better decisions that shape our architecture, coding style, and priorities.

### What I learned

+ Think at the memory level rather than just the application level.
+ Be mindful of copies, clones, and allocations (`vec![]`, `from_str`, `String::new`, `.clone()`).
+ Keep an open mind about types I already know but rarely think to use (`Cow`, `Cell`).
+ If the standard library already has an API that does exactly what I need, use it directly instead of reinventing the logic (e.g., iterator `position` over `read_until`).
+ Leverage `static` when applicable (`OnceLock` magic).
