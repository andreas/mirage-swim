# mirage-swim

This repo is the beginnings of an implementation of the [SWIM protocol](http://www.cs.cornell.edu/~asdas/research/dsn02-SWIM.pdf) for [Mirage](https://mirage.io/). The goal is to provide a scalable membership protocol for clusters of unikernels.

The intention is that unikernels in a cluster run the the Swim task alongside their actual task. The application task can then query Swim about the state of the cluster, e.g. to issue a request to another node.

# Usage

So far, this has only been tested on OSX running only the Swim task (no application task). Run the following to start a node locally:

```bash
env NET=direct DHCP=true mirage configure --unix
make
sudo ./mir-swim
```

(sudo is required to access vmnet)

You can then run the following in another shell to join the cluster:

```bash
sudo ./mir-swim --join_ip=[the ip assigned to the previous node]
```

You should see debug information in the console.

# Todo

- Testing and verification
- [SWIM protocol improvements](https://github.com/hashicorp/memberlist#changes-from-swim)
- Register metadata about nodes
- Interface to application task
