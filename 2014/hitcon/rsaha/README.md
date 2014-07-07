
# (crypto) rsaha [200]

## Description

Can you break RSA?
https://dl.dropbox.com/s/xqkoamfvas1rdb7/rsaha-fe50cf1bcae41e8ec6eeebccf3f0de7c.py
http://ctf.tw/rsaha-fe50cf1bcae41e8ec6eeebccf3f0de7c.py
nc 54.64.40.172 5454

### Hint

(none)

## Solution

### WriteUps

- https://github.com/ctfs/write-ups/tree/master/hitcon-ctf-2014/rsaha

### My Notes

1. 

### Takeaways

- 

### RSA

RSA Involves 3 steps, key generation, encryption, and decryption.

#### Key Generation

1. Choose two prime numbers, `p` and `q`.  They should be similar bit-length.
   Can be found efficiently using the primality test.
1. Compute `n = pq`.  `n` is used as the modulus.
1. Compute `φ(n) = φ(p)φ(q) = (p − 1)(q − 1) = n - (p + q - 1)`.  `φ` is Eulers
   totient function.
1. Choose an integer `e` such that `1 < e < φ(n)` and `gcd(e, φ(n)) = 1`.  That
   is to say, `e` and `φ(n)` are coprime.

     - `e` is the public key exponent.
     - `e` having a short bit-length and small Hamming weight results in more
       efficient encryption – most commonly `216 + 1 = 65,537`. However, much
       smaller values of `e` (such as 3) have been shown to be less secure in
       some settings.

1. Determine `d` as `d ≡ e−1 (mod φ(n))`; i.e., `d` is the multiplicative
   inverse of `e (modulo φ(n))`.
   - This is more clearly stated as: solve for `d` given `d⋅e ≡ 1 (mod φ(n))`
   - This is often computed using the extended Euclidean algorithm. Using the
	 pseudocode in the Modular integers section, inputs `a` and `n` correspond
	 to `e` and `φ(n)`, respectively.
   - `d` is kept as the private key exponent.
1. 
