# NetNomics

## A protocol for negotiating network payments

### William Yager

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

## Verifiable Reputation Indicators

### Non-Interactive Reputation Indicators

Merkle Proof-Of-Burn

### Interactive Reputation Indicators

Review Systems, Blacklists, Verifiable information-free Pings

## Compressed Utility Function

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
* $P_B$: Bandwidth selectivity: If your selectivity (unit: dimensionless) is high, there's a particular bandwidth at which the connection suddenly becomes useful and you really don't care at all beyond that. If your selectivity is low, there's a broad range of bandwidths at which the connection is useful, and you have a mild preference for higher bandwidths.
* $C_L$: Center latency: The average latency (unit: seconds) at which you are willing to pay 50% of maximum.
* $P_L$: Latency selectivity: How gradual your latency requirements are.
* $C_D$: Center latency deviation: The latency standard deviation (unit: seconds) at which you are willing to pay 50% of maximum.
* $P_D$: Deviation selectivity: How strict your latency bounds are. This will be highest for hard-real-time applications.
* $C_P$: Center packet loss: The packet loss (unit: dimensionless) at which you are willing to pay 50% of maximum.
* $P_P$: Packet loss selectivity: How strict your packet loss bounds are. Will typically be higher for real-time application where retransmits are expensive, like video games.

The smallest bandwidth supported ($B_{min}$) is $0.001$ bits/sec. The largest bandwidth supported ($B_{max}$) is $10^{24}$ bits/sec. (This protocol is intended to be future-proof and to serve bulk applications, and having wide margins allows for better optimization algorithm behavior towards the edges.)

The shortest latency supported ($L_{min}$) is $1$ nanosecond. The longest latency supported  ($L_{max}$)is $10^{10}$ seconds. (In case anyone wants to use the protocol for deep-space purposes.)

The lowest latency standard deviation supported  ($D_{min}$) is $0.1$ nanoseconds. The largest latency standard deviation supported ($D_{max}$) is $10^9$ seconds.

The lowest packet loss supported ($P_{min}$) is $10^{-30}$. The highest packet loss supported ($P_{max}$) is 1 (i.e. all packets lost).

The highest selectivity supported in all categories ($S_{max}$) is 1000 (effectively a hard cut-off). The smallest selectivity supported ($S_{min}$) is $0.1$ (little selectivity across many orders of magnitude).

The following equations can be re-expressed in a number of ways. I chose the way that (I hope) best matches the justification I gave above. You will notice some obvious simplifications; these simplifications may be followed in implementation, but may make the equations harder to understand.

Total utility, which maps from bandwidth $b \in [B_{min}, B_{max}]$, latency $l \in [L_{min}, L_{max}]$, latency deviation $d \in [D_{min}, D_{max}]$, and packet loss $p \in [P_{min}, P_{max}]$ to $[0,1)$ is defined as:

$$U(b,l,d,p) = U_B(b) U_L(l) U_D(d) U_P(p)$$

Bandwidth utility:

$$U_B(b) = \frac{1}{1+e^{-P(ln(b/B_{min})-ln(C/B_{min}))}}$$

Bandwidth selectivity: 

$$P = S_{min} * \Big(\frac{S_{max}}{S_{min}}\Big)^{P_B}$$

Bandwidth center: 

$$C = B_{min} * \Big(\frac{B_{max}}{B_{min}}\Big)^{C_B}$$

Simplified: $\frac{1}{\frac{C}{b}^P + 1}$.

Notice the exponent's sign is negative, indicating larger values are better.

Latency utility:

$$U_L(l) = \frac{1}{1+e^{P(ln(l/L_{min})-ln(C/L_{min}))}}$$
$$P = S_{min} * \Big(\frac{S_{max}}{S_{min}}\Big)^{P_L}$$
$$C = L_{min} * \Big(\frac{L_{max}}{L_{min}}\Big)^{C_L}$$


Notice the exponent's sign is positive, indicating smaller values are better.

Latency deviation utility:

$$U_D(d) = \frac{1}{1+e^{P(ln(d/D_{min})-ln(C/D_{min}))}}$$
$$P = S_{min} * \Big(\frac{S_{max}}{S_{min}}\Big)^{P_D}$$
$$C = D_{min} * \Big(\frac{D_{max}}{D_{min}}\Big)^{C_D}$$


Packet loss utility:

$$U_P(p) = \frac{1}{1+e^{-P(ln(p/P_{min})-ln(C/P_{min}))}}$$
$$P = S_{min} * \Big(\frac{S_{max}}{S_{min}}\Big)^{P_P}$$
$$C = P_{min} * \Big(\frac{P_{max}}{P_{min}}\Big)^{C_P}$$

Note that the cost doesn't actually show up anywhere in these equations. This is due to assumption 1, which implies that we only have to define the *relative* utility of different connection options. 




We can "compress" this utility function by packing each of $C_B, P_B, C_L, P_L, C_D, P_D, C_P, P_P$ into a small (say, 16-bit) unsigned integer, representing a fixed-point value in $[0,1)$. Because of the structure of our equations, the density of the functions as generated by $C/P_{B/L/D/P}$ is well-distributed in log-space and we don't "waste" any bits.



## Government Bandwidth Sales

## FTRs