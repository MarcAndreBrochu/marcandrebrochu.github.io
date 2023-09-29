---
title: L'existence et l'unicité de l'extension en série de Laurent
tags: analyse complexe, notes
---

Soit $\Omega$ un domaine et $f:\Omega\to\mathbb{C}$ une fonction holomorphe.

Je prend un $z\in A$

$\gamma_1(t) = r_1e^{-it}$ et $\gamma_2(t) = r_2e^{it}$.

(Expliquer) Par la formule intégrale de Cauchy on a que
\begin{equation}
f(z) = \frac{1}{2\pi i}\left(\oint_{\gamma_1} \frac{f(w)}{w-z}dz + \oint_{\gamma_2} \frac{f(w)}{w-z}dz\right)
\end{equation}

# Existence de l'extension

## Développement de la partie principale

Lorsque $w$ est sur le chemin $\gamma_1$, on a que
\begin{align}
\frac{1}{w-z} &= \frac{1}{(w-a)-(z-a)} = \frac{1}{w-a}\cdot\frac{1}{1-\frac{z-a}{w-a}}\\
&= \frac{1}{w-a}\frac{-(w-a)}{z-a}\sum_{n=0}^{\infty}\left(\frac{w-a}{z-a}\right)^n\\
&= \frac{-1}{w-a}\sum_{n=1}^\infty \left(\frac{w-a}{z-a}\right)^n
\end{align}
puisque $|\frac{z-a}{w-a}|>1$ il faut utiliser l'autre formule pour une série géométrique.

Puisque $w$ est sur le chemin $\gamma_1$, la distance entre $w$ et $a$ est plus petite que celle entre $z$ et $a$. Donc pour tout $w$, $|\frac{w-a}{z-a}|\leq \rho < 1$ pour un certain $\rho$ réel positif et $|\frac{w-a}{z-a}|^n\leq \rho^n$ pour tout $n$. Comme $f$ est continue sur $\gamma_1$, elle est localement bornée et par compacité de $\gamma_1$ bornée par un certain réel $C$. Donc pour tout $w\in\gamma_1$ et pour tout $n$,
\begin{equation}
\left|\frac{f(w)}{w-a}\cdot\left(\frac{w-a}{z-a}\right)^n\right|\leq C\rho^n
\end{equation}

Par le test M de Weierstrass, la série $$\sum_{n=1}^\infty \frac{f(w)(w-a)^{n-1}}{(z-a)^n}$$ converge absolument et uniformément sur $\gamma_1$.

On peut commuter l'intégration et la somme:
\begin{align}
\frac{1}{2\pi i}\oint_{\gamma_1} \frac{f(w)}{w-z}dw &= \frac{1}{2\pi i}\oint_{\gamma_1}\sum_{n=1}^\infty\frac{-f(w)(w-a)^{n-1}}{(z-a)^n}dw\\
&= \frac{-1}{2\pi i}\sum_{n=1}^\infty \oint_{\gamma_1} \frac{f(w)(w-a)^{n-1}}{(z-a)^n}dw\\
&= \frac{1}{2\pi i}\sum_{n=-\infty}^{-1}\oint_{-\gamma_1} \frac{f(w)}{w-a}\left(\frac{z-a}{w-a}\right)^ndw\\
&= \sum_{n=-\infty}^{-1}(z-a)^n\frac{1}{2\pi i}\oint_{-\gamma_1} \frac{f(w)}{(w-a)^{n+1}}dw
\end{align}

## Développement de la partie de Taylor

En procédant de façon similaire à la partie précédente, je trouve que
$$ \frac{f(w)}{w-z} = \frac{f(w)}{w-a}\sum_{n=0}^\infty \left(\frac{z-a}{w-a}\right)^n$$ converge absolument et uniformément sur $\gamma_2$.

Donc
\begin{align}
\frac{1}{2\pi i}\oint_{\gamma_2}\frac{f(w)}{w-z}dw &= \frac{1}{2\pi i}\oint_{\gamma_2}\frac{f(w)}{w-a}\sum_{n=0}^\infty\left(\frac{z-a}{w-a}\right)^ndw\\
&= \frac{1}{2\pi i}\sum_{n=0}^\infty \oint_{\gamma_2}\frac{f(w)(z-a)^n}{(w-a)^{n+1}}dw\\
&= \sum_{n=0}^\infty (z-a)^n\frac{1}{2\pi i}\oint_{\gamma_2}\frac{f(w)}{(w-a)^{n+1}}dw
\end{align}

## On recolle tout

\begin{align}
f(z) &= \frac{1}{2\pi i}\left(\oint_{\gamma_1} \frac{f(w)}{w-z}dz + \oint_{\gamma_2} \frac{f(w)}{w-z}dz\right)\\
&= \sum_{n=-\infty}^{-1}(z-a)^n\frac{1}{2\pi i}\oint_{-\gamma_1} \frac{f(w)}{(w-a)^{n+1}}dw + \sum_{n=0}^\infty (z-a)^n\frac{1}{2\pi i}\oint_{\gamma_2}\frac{f(w)}{(w-a)^{n+1}}dw
\end{align}

Puisque $-\gamma_1$ est homotope à $\gamma_2$ dans l'anneau, l'intégrale d'une même fonction le long de ces chemins donne le même résultat. Ainsi on peut écrire $$f(z)=\sum_{n=-\infty}^\infty c_n(z-a)^n$$
où
$$c_n = \frac{1}{2\pi i}\oint_\gamma \frac{f(w)}{(w-a)^{n+1}}dw$$ avec $\gamma$ un lacet quelconque dans l'anneau.

# Unicité de l'extension

On suppose qu'on a deux extensions en série de Laurent pour $f$ en $z$:
$$f(z) = \sum_{n=-\infty}^\infty c_n(z-a)^n = \sum_{n=-\infty}^\infty c_n'(z-a)^n$$

On multiplie des deux côtés par $(z-a)^{-k-1}$ pour $k$ un entier arbitraire et on intègre sur $\gamma$ un lacet dans l'anneau.
$$\oint_\gamma \sum_{n=-\infty}^\infty c_n(z-a)^{n-k-1}dz = \oint_\gamma \sum_{n=-\infty}^\infty c_n'(z-a)^{n-k-1}dz$$

Ces séries convergent absolument et uniformément sur $r+\varepsilon\leq |z-a|\leq R-\varepsilon$ où $\varepsilon$ est un nombre assez petit pour que $\gamma$ soit complètement contenu dans l'anneau restreint. Donc l'intégration et la sommation peuvent être interchangées:
$$\sum_{n=-\infty}^\infty c_n\oint_\gamma (z-a)^{n-k-1}dz = \sum_{n=-\infty}^\infty c_n'\oint_\gamma (z-a)^{n-k-1}dz$$
et l'identité
$$\oint_\gamma (z-a)^{n-k-1}dz = 2\pi i\delta_{nk}$$
avec $\delta_{ij}$ le delta de Kronecker, permet de conclure.
