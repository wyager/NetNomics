---
title: 'NetNomics: A Protocol for Bandwidth Markets'
author:
- William Yager
---

## Motivation

Due to increasing censorship pressure, many technologists are developing systems designed from the ground up for censorship resistance. One of the most challenging systems to harden against censorious attackers is the physical communications infrastructure. Several efforts to this end exist [cite], but have met little success, primarily because they rely on technical enthusiasm and goodwill to gain participants. 

Besides overt political censorship pressure, there is also censorship pressure from data carriers' financial interests. Due to a lack of competition, data carriers can (and, in some cases, do) benefit financially from raising prices on data connections to competitors. Legal efforts to curb this practice have undesirable second-order effects.

One of the primary challenges with establishing censorship-resistant communications infrastructure is a lack of accurate price signals. Distributed networking efforts lack a sensible pricing mechanism, and ISP competition (for the most part) does not exist at a regional level due to irrational government fiber easement policies and a similar lack of pricing mechanisms.

This paper proposes a simple and powerful payment negotiation mechanism geared towards the purchase of network access from decentralized bandwidth brokers. The protocol described herein, NetNomics, is fully compatible with security, privacy, and sound economics. NetNomics works for all varieties of bandwidth brokerage, from cell service to satellite communications. 

## State of The World

Current consumer-oriented bandwidth brokers (ISPs, cell carriers, etc.) offer extremely limited options when it comes to connection behavior. In the case of cell phones, consumers have precisely two options: a low-cost, medium-bandwidth, medium-latency data network, or a high-cost, low-bandwidth, low-latency voice-only network. This suffices for many use cases, but fails to accurately capture the preferences of bandwidth market actors. For example, if a user is downloading a large file, they may prefer a very cheap connection with no care for latency over the more expensive medium-latency data network available to their cell phone. If the user's download is going to take 10 minutes anyway, they don't really care if the packets take an extra 500ms to reach their device. As of now, it is impossible to express this preference to the network provider, so users end up overpaying for bandwidth. In an ideal world, cell carriers could de-prioritize such connections in exchange for a per-bit discount.

The situation is somewhat better at the industrial level; large bandwidth clients can negotiate data carrier SLAs that allow for variable pricing based on network characteristics. A fat, high-latency connection can be purchased separately from a thin, ultra-low-latency connection. However, the situation could still be vastly improved.

This protocol aims to capture bandwidth consumer preferences in an efficient and accurate manner. If successful, this has the potential to vastly improve user expereince and market efficiency. 

## Protocol

The high-level protocol description is very simple. This paper intentionally avoids specifying particular algorithms or payment systems, but offers some currently practical suggestions in later sections.

There are two participants in a NetNomics transaction; the bandwidth consumer and the bandwidth provider. The bandwidth consumer wishes to connect to the internet, and the bandwidth provider has a connection to the internet (possibly also purchased via NetNomics).

The consumer, Carol, has a (possibly ephemeral) assymetric keypair $P_C$ (public) and $S_C$ (secret). The producer, Patrick, similarly has a keypair $P_P$ and $S_P$. These keypairs can be entirely ephemeral, although (as we will see later) typically only the consumer will use ephemeral keys. Consumers can protect their privacy by regularly changing their keypair.

1. Carol and Patrick swap $P_C$ and $P_P$ and establish an encrypted and authenticated channel using their keys. An MiTM attack is obviously possible here if Carol and Patrick have never done business before. However, it does not matter, as this protocol is built on the assumption that Patrick may be hostile. 

2. Patrick and Carol swap any [verifiable reputation indicators] they care to. Typically Patrick will have a lower requirement for Carol's reputation than vice-versa, as Carol will be extending Patrick some (small) amount of credit and not vice-versa. The dynamics are the same as any other vendor/consumer relationship. Any [non-interactive reputation indicators] will help Carol to establish the value of Patrick's reputation

3. If both Patrick and Carol decide to move forward with the negotiation, both perform a small [mutual interactive penance]. This step costs both Carol and Patrick some very small amount of money. The minimum cost of electricity associated with Patrick's penance should exceed Carol's maximum financial exposure. This makes "spamming" fake bandwidth buy/sell offers slow and expensive, and makes it impossible to make money through fraudulent bandwidth sale offers. 

4. At this point, Patrick may allow Carol limited network access, so as to validate any [interactive reputation indicators] he provided in step 2. This will further help Carol to establish the value of Patrick's reputation.

5. Carol sends Patrick her [compressed utility function]. Patrick considers the connections he has available to sell to Carol and chooses one that maximizes Carol's utility. He sends the [connection details] to Carol.

6. Carol compares this offer with offers she has received from other bandwidth vendors. WLOG, assume that Patrick's offer gives her the highest utility. Carol indicates to Patrick that she wants to move forward with the purchase.

7. Patrick allows Carol sufficient network access to begin a [payment].

8. Carol sends Patrick a small payment.

9. Patrick credits Carol with the corresponding amount of unrestricted bandwidth.

10. When Carol is running low on bandwidth, go back to step 8.

## Mutual Interactive Penance

In order to make it economically unfavorable for parties to defraud their counterparties, we make entering into a transaction slightly more expensive than the maximum financial exposure of either party during the transaction.

To do so, we choose some challenge-response proof-of-work algorithm parametrized over a difficulty. Carol and Patrick issue each other random challenges. Both solve the proof-of-work algorithm at a low difficulty and respond with a solution. Carol and Patrick now repeat this process, but with a higher difficulty. This continues several times until Carol and Patrick have both invested a sufficiently large (expected) amount of computational effort.

The chosen proof-of-work algorithm should not be solvable at substantially lower cost on custom hardware. 

## Verifiable Reputation Indicators

The goal of a Verifiable Reputation Indicator is to trustlessly prove some form of credit-worthiness to a counterparty. In other words, you want to prove to someone you don't know that you won't try to scam them. Some techniques for doing so are described here.

### Non-Interactive Reputation Indicators

These are Reputation Indicators that don't require any communication with the outside world. These are primarily useful to the person purchasing bandwidth, as they allow them to check the credit-worthiness of the bandwidth merchant before they've been connected to the outside world.

#### Merkle Proof-Of-Burn

The general approach is based on SPV, or "Simplified Payment Verification" as described by Nakamoto in the original Bitcoin whitepaper.

To demonstrate creditworthiness, Patrick creates a Bitcoin transaction that provably "destroys" the Bitcoins and is uniquely linked to $P_P$. For example, Patrick could create an OP_RETURN transaction containing the hash of $P_P$ with some non-trivial amount of Bitcoin as input. Patrick would permanently lose the Bitcoins, and this transaction would be linked to him uniquely.

If Patrick ever attempts to scam Carol, she can blacklist Patrick by his pubkey. Patrick can never scam Carol again without burning more Bitcoins. When Carol gets internet access, she can publish a bad review associated with Patrick's pubkey, and Patrick's reputation (which he paid at least some amount of money for) is tarnished for those people who trust Carol. This is essential to the usefulness of several Interactive Reputation Indicators discussed later.

This is non-interactive because, using SPV, Patrick can prove to Carol (to a high degree of confidence) that he made such a transaction by giving Carol only a small amount of data; there is no need for a full connection to the Bitcoin network.

### Interactive Reputation Indicators

These are evidence of credit-worthiness that require a (limited) connection to the outside world. 

#### Review Systems

Just as now, vendors would be subject to public review. Vendor identities would be tied to their public keys. Review systems could either be highly centralized and curated (as they are now) or more decentralized and automated (a la web-of-trust).

#### Blacklists

Curated blacklists of bad nodes (as we have now for web domains that serve malware) would require scammy vendors to continually change their identity, at great expense.

#### Verifiable Information-Free Pings

In order to test the (claimed) network properties of a connection, a consumer needs to be able to send something through the network. However, vendors don't want unscrupulous consumers to abuse this feature and send data without paying. (This has happened in real life with users tunneling traffic over ICMP to avoid paying for network access.)

The general protocol is as follows:

<stuff here>

## Consumer Utility Function

In order to offer the best possible service to a bandwidth consumer and to accurately propagate market information, it is necessary to accurately capture the consumer's utility function. In order to make this efficient to communicate and optimize for, we make several reasonable assumptions about the consumer's utility function on bandwidth offers.

1. The consumer's utility is linear with cost. That is, spending 2 cents is twice as bad as spending 1 cent. This is reasonable because real utility functions are locally linear with cost and the amounts of money involved are small. 

2. The consumer's utility is sigmoidal in the log-space of available bandwidth, latency, latency deviation, and packet loss. This is will not be perfectly accurate, but is extremely flexible, simple to compute and optimize, and closely matches intuitive preferences.

3. Higher bandwidth, lower latency, lower latency deviation, and less packet loss are all good to some degree. Not many people would disagree with any of these.

For example, consider the use case of a phone call. Humans dislike high latencies during vocal conversation, as it makes it hard to coordinate pausing and listening. A latency of 500ms is more or less tolerable, but we would really prefer something more like 50ms if it's not much more more expensive. However, we don't notice nearly as much of a difference between 50ms and 5ms, so we're not willing to pay very much to shave off those extra few milliseconds. The following sigmoid is a very reasonable approximation of the relative value a bandwidth consumer might assign to different latency options.

![Latency example](lat.png)

The same is true of available bandwidth. If you're streaming video, too low bandwidth is basically worthless to you. As you get more bandwidth, you can stream higher quality video, which you prefer to some degree. At some point, the available bandwidth exceeds the highest video quality's required bandwidth, so you don't really care anymore. If you're downloading a large file, like an operating system image, you may have a slight preference for dowloading the image in 1 minute instead of 2. This can be represented with a relatively flat sigmoid, like


![Bandwidth example](band.png)

In other words, you're OK with a ~100mb/sec connection, but you'd pay ~50% more for a ~1000mb/sec connection. 

Again for latency deviation and packet loss; if you're making a phone call, you really don't want sudden changes in latency or dropped audio packets. On the other hand, if you're downloading large files via a resilient protocol like BitTorrent, you don't really care about either of those things above some reasonable threshold.

Some representative examples of tolerable conditions include:

* Phone calls: Low bandwidth, low latency, low deviation, low packet loss.
* Torrents: High bandwidth, high latency, high deviation, high packet loss.
* Video streaming: High bandwidth, moderate latency, moderate deviation, low packet loss.


Based on this rationale, the utility function is parametrized by 8 numbers:

* $C_B$: Center bandwidth: The bandwidth (unit: bits per second) at which you are willing to pay 50% of maximum.
* $S_B$: Bandwidth selectivity: If your selectivity (unit: dimensionless) is high, there's a particular bandwidth at which the connection suddenly becomes useful and you really don't care at all beyond that. If your selectivity is low, there's a broad range of bandwidths at which the connection is useful, and you have a mild preference for higher bandwidths. A negative selectivity implies higher values are better.
* $C_L$: Center latency: The average latency (unit: seconds) at which you are willing to pay 50% of maximum.
* $S_L$: Latency selectivity: How gradual your latency requirements are.
* $C_D$: Center latency deviation: The latency standard deviation (unit: seconds) at which you are willing to pay 50% of maximum.
* $S_D$: Deviation selectivity: How strict your latency bounds are. This will be highest for hard-real-time applications.
* $C_P$: Center packet loss: The packet loss (unit: dimensionless) at which you are willing to pay 50% of maximum.
* $S_P$: Packet loss selectivity: How strict your packet loss bounds are. Will typically be higher for real-time application where retransmits are expensive, like video games.

The utility function is defined as follows:

$$U(b,l,d,p) = U_B(b) U_L(l) U_D(d) U_P(p)$$

Individual utilities:

$$U_N(x) = \frac{1}{1+(\frac{x}{C_N})^{S_N}}$$

Each individual utility function forms a sigmoid on a graph of price-vs-x where the x axis is spaced logarithmically, as we wanted.

### Utility Function Compression

The utility function described above is elegant and easy to compute. Some special consideration is due as the inputs to the function can span many orders of magnitude; for example, both 10kbps and 10Gbps connections are reasonably common at the time of writing (depending on what kind of machines you're working with). Even common consumer devices like cell phones regularly experience bandwidth changes by factors of thousands.

To accomodate this fact, we can represent physical values (bandwidth, time, etc.) logarithmically rather than linearly. For each physical quantity (bandwidth, latency, latency deviation, packet loss percentage) we pick a (mostly arbitrary) value that corresponds to a logarithm of 0 and an (also mostly arbitrary) value that corresponds to a logarithm of 1. For reasons that will become clear shortly, I will arbitrarily prescribe the following values:

For bandwidth, $B_0 = 10^{-3} \frac{b}{s}$ and $B_1 = 10^{24} \frac{b}{s}$. 

For latency, $L_0 = 10^{-9} s$ and $L_1 = 10^{10} s$. 

For latency deviation, $D_0 = 10^{-10} s$ and $D_1 = 10^{11} s$. 

For packet loss, $D_0 = 10^{-30}$ and $D_1 = 1$. 

For selectivity, $S_0 = 0.1$ and $S_1 = 1000$.

For any physical quantity $Q$, $exp_Q(x) = Q_0 * (\frac{Q_1}{Q_0})^x$.

$exp_Q$ has the convenient property that it maps values in the range $[0,1)$ to realistic physical values without any large gaps.

Therefore, we can represent any realistic bandwidth, latency, deviation, or packet loss to extremely high precision with a single value in the range $[0,1)$. This is straightforwardly represented by a Q0.32 unsigned fixed-point binary number.

If we weren't to use this logarithmic representation and instead were to use e.g. an integer corresponding to the number of bits per second, we would either have to use a huge integer (to capture values as large as $10^{24}$) or have abysmal accuracy towards the smaller end of admissible values. On the other hand, using this logarithmic representation gives us excellent accuracy across the entire range of physically realistic values.

Using this technique, we can represent our entire utility function to extremely high accuracy using 8 32-bit fixed-point numbers ($C/S_{B/L/D/B}$) and the connection characteristics using 4 32-bit fixed-point numbers ($b/l/d/b$). 

A dimensionally-checked reference implementation in Haskell (with equational transformations informally verified in Coq over the reals) is available at <add link>


## Government Bandwidth Sales

This protocol is designed to help the market remove inefficiencies in bandwidth allocation. However, it can only do so much in the presence of a pessimizing legal framework.

The situation is particularly bad in the case of wireless bandwidth allocations. However, I beleive that with only a minimal amount of work, radio frequency regulatory bodies could vastly improve the efficiency of wireless bandwidth allocation.

Right now, bandwidth is sold in huge chunks to small groups of bidders, and only very infrequently. This is obviously not an effective mechanism for communicating market information, and it shows; there is almost no competition in the wireless carrier market due to this legal framework, and customer satisfaction with wireless carriers is extremely low.

If RF regulatory bodies (for example, the FCC) were instead to automatically run frequent regional bandwidth auctions (on the scale of miles, minutes, and megahertz), small enterprises could (using this protocol) efficiently sell bandwidth to local consumers. Anyone could efficiently determine the current going rate of data in an area, figure out how much it would cost for them to do better, and make decisions accordingly. For example, an individual could easily determine if it would make them money (and, by fairly direct reasoning, optimize bandwidth allocation) to get easements to run a fiber line or configure a microwave link to a neighboring town.
