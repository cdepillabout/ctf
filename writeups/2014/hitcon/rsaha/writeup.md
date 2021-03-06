---
title: (crypto) rsaha [200]
---

## Description

Can you break RSA?

[rsaha-fe50cf1bcae41e8ec6eeebccf3f0de7c.py](rsaha-fe50cf1bcae41e8ec6eeebccf3f0de7c.py)

nc 54.64.40.172 5454

### Hint

(none)

## Solution

### WriteUps

- [ctfs rsaha writeup](https://github.com/ctfs/write-ups/tree/master/hitcon-ctf-2014/rsaha)

### My Notes

**TODO**

1.

### Takeaways

**TODO**

-

### RSA

RSA Involves 3 steps, key generation, encryption, and decryption.

#### Key Generation

1. Choose two prime numbers, \\(p\\) and \\(q\\).  They should be similar bit-length.
   Can be found efficiently using the primality test.

1. Compute \\(n = pq\\).  \\(n\\) is used as the modulus.

1. Compute \\(\\varphi(n) = \\varphi(p)\\varphi(q) = (p − 1)(q − 1) = n - (p + q - 1)\\).
   \\(\\varphi\\) is Eulers totient function.

1. Choose an integer \\(e\\) such that \\(1 < e < \\varphi(n)\\) and
   \\(gcd(e, \\varphi(n)) = 1\\).  That is to say, \\(e\\) and \\(\\varphi(n)\\) are coprime.
     - \\(e\\) is the public key exponent.
     - \\(e\\) having a short bit-length and small Hamming weight results in more
       efficient encryption – most commonly \\(216 + 1 = 65,537\\). However, much
       smaller values of \\(e\\) (such as 3) have been shown to be less secure in
       some settings.

1. Determine \\(d\\) as \\(d \\equiv e^{-1} \\pmod{\\varphi(n)}\\);
   i.e., \\(d\\) is the multiplicative inverse of \\(e \\pmod{\\varphi(n)}\\).
   - This is more clearly stated as: solve for \\(d\\) given
     \\(d \\cdot e \\equiv 1 \\pmod{\\varphi(n)}\\)
   - This is often computed using the [extended Euclidean
	 algorithm](http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm).
	 Using the pseudocode in the Modular integers section, inputs \\(a\\) and \\(n\\)
	 correspond to \\(e\\) and \\(\\varphi(n)\\), respectively.
   - \\(d\\) is kept as the private key exponent.

- *public key* consists of modulus \\(n\\) and public exponent \\(e\\).

- *private key* consists of modulus \\(n\\) and private exponent \\(d\\).
  \\(p\\), \\(q\\), and \\(\\varphi(n)\\) must also be kept secret because
  they can be used to calculate \\(d\\).

**TODO**

#### Encryption

1. Alice transfers her public key \\((n, e)\\) to Bob and keeps the
   private key \\(d\\) secret.

1. Bob wants to send \\(m\\) to Alice (such that \\(0 ≤ m < n\\)).

1. Bob computes \\(c\\) as \\(c \\equiv m^e \\pmod{n}\\).  This can be done quickly using
   exponentiation by squaring.

**TODO**

#### Decryption

Alice can recover \\(m\\) from \\(c\\) by using her private key exponent \\(d\\) by
computing \\(m \\equiv c^d \\pmod{n}\\).

(There are more efficient ways of calculating \\(c^d\\) by using the [Chinese
remainder algorithm](http://en.wikipedia.org/wiki/RSA_%28algorithm%29#Using_the_Chinese_remainder_algorithm).)

**TODO**

#### [Extended Euclidean Algorithm](http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)

**TODO**


#### [Exponentiation by Squaring](http://en.wikipedia.org/wiki/Exponentiation_by_squaring)

This allows us to efficiently calculate the encryption and decryption of a
message like \\(c \\equiv m^e \\pmod{n}\\).

**TODO**

#### [Chinese Remainder Algorithm for Calculating `m` from `c`](http://en.wikipedia.org/wiki/Chinese_remainder_theorem)

For efficiency, the following values are are precomputed and stored as part of
the private key.

- \\(p\\) and \\(q\\): the primes from the key generation.
- \\(d_p = d \\pmod{p - 1}\\)

**TODO**

