# Sonnet

Sonnet is a systems programming language that strives to be both principled and to execute every idea to its perfection. It aims to be a complete language where every component fits together seamlessly; if a feature cannot be implemented perfectly, it is discarded.

Many languages suffer from a gap between what is possible within their native syntax and what can only be achieved externally (or through "unsafe" blocks, as in Rust). Sonnet, however, is designed to be capable of everything. It achieves this by doing nothing on its own, ensuring it never hinders the programmer. This philosophy naturally excludes features like a borrow checker or garbage collector.

Sonnet is a simple language in the sense of a language from which nothing can be removed, and one that performs no action unless explicitly commanded by the user.

# Ginko

Ginko is a medium-level language designed to be a simpler replacement for C and LLVM. Based on an infinite-register machine, it is platform-independent while granting programmers the greatest possible freedom to define how a program executes close to the way assembly works. Ginko will be used as the backend for Sonnet.