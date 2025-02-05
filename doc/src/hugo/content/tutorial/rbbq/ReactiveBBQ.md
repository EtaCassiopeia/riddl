---
title: "Reactive BBQ"
type: "page"
draft: "false"
weight: 30
---

```riddl
Render https://github.com/reactific/riddl-examples/blob/main/src/riddl/ReactiveBBQ/ReactiveBBQ.riddl
```

Everything in RIDDL revolves around creating domains and subdomains. These
are logical groupings of definitions that *belong* together, presumably
because they mimic and organizations structure or some other logical, real
world groupings. Domains can be nested.

At this top level of definition we can see that a single 
[`domain`]({{< relref "../../concepts/domain.md" >}}) 
named `ReactiveBBQ` represents the entire enterprise. The details of that 
top level [`domain`]({{< relref "../../concepts/domain.md" >}}) is abstracted
away via three [`include`]({{< relref "../../concepts/include.md" >}}) 
statements within its body, one for each of the subdomains: 
* [`Restaurant`]({{< relref "restaurant" >}})
* [`Back Office`]({{< relref "backoffice" >}})
* [`Corporate`]({{< relref "corporate" >}})


