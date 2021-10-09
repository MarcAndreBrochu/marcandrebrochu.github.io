---
title: A perfect number theory proof
tags: maths, proof
---

For my first article on this blog (or any blog for that matter), I want to present one of my favorite proofs of elementary number theory. It's based around the concept of a *perfect number*. You know that a concept named like that ought to be interesting. A perfect number is any positive integer that is equal to the sum of its positive divisors, except the fact that you don't count the number itself in that sum. You only sum its *proper* divisors. The whole is equal to the sum of its parts; that's why such a number is *perfect*. For instance, six is a perfect number because its proper divisors are 1, 2 and 3; if you sum these up you get back six. In fact six is the smallest perfect number. The next one is 28 as its proper divisors are 1, 2, 4, 7 and 14.

> Six is a number perfect in itself, and not because God created all things in six days; rather, the converse is true. God created all things in six days because the number is perfect.
>
> -- <cite>The City of God by Saint Augustine</cite>

# The Euclid-Euler theorem
If we introduce the notation $\sigma(n)$ to mean "the sum of all the positive divisors of $n$, including $n$ itself", then a number $n$ is perfect if and only if $\sigma(n)=2n$. For instance, the obvious fact that a prime number cannot be perfect can be translated as saying that for all primes $p$, we have $\sigma(p)=p+1$, which is never equal to $2p$.

An interesting fact about $\sigma$ is that it's a *multiplicative* function. It means that if $n$ and $m$ are two coprime numbers (they have no prime divisors in common), then $\sigma(nm)=\sigma(n)\sigma(m)$. Intuitively, that happens because the set of divisors of $n$ is essentially disjoint from the set of divisors of $m$ --- a consequence of coprimality. Therefore each divisor of $nm$ can be expressed as a product $d_1d_2$, where $d_1$ divides $n$ and $d_2$ divides $m$. Since you can express a sum of products as a product of sums, well, you get the idea.

## Euclid's part
In his *Elements*, three hundred years before Jesus Christ was born, Euclid proved  that if $2^p-1$ is prime, then any number $n$ of the form $2^{p-1}(2^p-1)$ must be perfect. Using our characterization of perfect numbers using $\sigma$ and the fact that it's a multiplicative function, that's easy to prove:

$$\begin{align}\sigma(n)&=\sigma(2^{p-1})\sigma(2^p-1)\\&=(\sum_{k=1}^{p-1}2^k)\cdot2^p\\&=(2^p-1)\cdot2^p\\&=2n.\end{align}$$

A prime number of the form $2^p-1$ is called a *Mersenne prime*. Euclid's result means that each Mersenne prime gives rise to a perfect number. It is not known whether or not there are infinitely many Mersenne primes, but if there were, it would prove that there are infinitely many perfect numbers.

## Euler's part
About two millenia after Euclid, Euler proved that any even perfect number must be of the form $2^{p-1}(2^p-1)$ with $2^p-1$ prime. To prove this, he used a simple proof with a very clever idea at a key moment. I still remember the first time I saw this proof. I still find it fascinating. Each simple idea flowing into the other, a logical path, a boat that derives. We start with $n$, an even perfect number. First, we compute $\sigma(n)$ in two different but equal ways. The first way is trivial: because $n$ is perfect, we know that $\sigma(n)=2n$. The second way uses the fact that $n$ is even: we write $n$ as $2^{k-1}m$, with $k>1$ and $m$ odd. Now

$$\begin{align}\sigma(n)&=\sigma(2^{k-1})\sigma(m)\\&=(2^k-1)\sigma(m).\end{align}$$

The two ways of looking at $\sigma(n)$ are equal, therefore

$$2^km = (2^k-1)\sigma(m).$$

We get two things from this equation. Firstly, since $2^k-1$ is coprime to $2^k$, we know that $2^k-1$ must divide $m$. We write $m=(2^k-1)M$ for some integer $M$. Secondly, we get that $\sigma(m)=2^kM$ just by dividing both sides by $2^k-1$ and substituting $(2^k-1)M$ for $m$. Putting these two things together, we have $\sigma(m)=m+M$.
Here is the clever part! This means that $m$ has only *two* divisors. The only integers with exactly two divisors are the primes, and so $m$ is a prime and $M=1$. By substituting $1$ for $M$ in the above equation, we finally obtain $m=2^k-1$. Note that $M$ is different from $m$ since $m = (2^k-1)M > (2^1-1)M$. That's in fact why we need $n$ to be even, because it guarantees that $k>1$.

## Putting it together
By taking Euclid's and Euler's parts and putting them together, we get a characterization of the even perfect numbers! An even number $n$ is perfect if and only if there exists a prime number $2^p-1$ such that

$$n = 2^{p-1}(2^p-1).$$

This is called the Euclid-Euler theorem. Interestingly, it is unknown if there are any odd perfect numbers... but if there are, the smallest one must be greater than $10^{150}$.

As I mentioned briefly above, that means that you can use Mersenne primes to generate perfect numbers. From GIMPS, the Great Internet Mersenne Prime Search, we know for instance that

$$2^{77232917}-1$$

is a prime. Therefore,

$$2^{77232916}(2^{77232917-1})$$

is perfect. It ends with 6.
