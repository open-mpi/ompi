/*
 * Copyright 2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright 2007 Nuova Systems, Inc.  All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#ifndef _KCOMPAT_H_
#define _KCOMPAT_H_

#ifdef __GNUC__
#  define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
#  define UNUSED(x) UNUSED_ ## x
#endif

#if !defined(__WINDOWS__)
#if defined(__KERNEL__)

#include <linux/version.h>
#include <linux/netdevice.h>
#include <linux/skbuff.h>

#ifndef PCI_VENDOR_ID_CISCO
#define PCI_VENDOR_ID_CISCO	0x1137
#endif

#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 21))
#define ENIC_AIC
#endif

#if !defined(__VMKLNX__)
#define RX_COPYBREAK
#endif

/*
 * Kernel backward-compatibility definitions
 */

#ifndef ioread8
#define ioread8 readb
#endif

#ifndef ioread16
#define ioread16 readw
#endif

#ifndef ioread32
#define ioread32 readl
#endif

#ifndef iowrite8
#define iowrite8 writeb
#endif

#ifndef iowrite16
#define iowrite16 writew
#endif

#ifndef iowrite32
#define iowrite32 writel
#endif

#ifndef DIV_ROUND_UP
#define DIV_ROUND_UP(n, d) (((n) + (d) - 1) / (d))
#endif

#ifndef DMA_BIT_MASK
#define DMA_BIT_MASK(n)	(((n) == 64) ? ~0ULL : ((1ULL<<(n))-1))
#endif

#ifndef NETIF_F_GSO
#define gso_size tso_size
#endif

#ifndef NETIF_F_TSO6
#define NETIF_F_TSO6 0
#endif

#ifndef NETIF_F_TSO_ECN
#define NETIF_F_TSO_ECN 0
#endif

#ifndef CHECKSUM_PARTIAL
#define CHECKSUM_PARTIAL CHECKSUM_HW
#define CHECKSUM_COMPLETE CHECKSUM_HW
#endif

#ifndef IRQ_HANDLED
#define IRQ_HANDLED 1
#define IRQ_NONE 0
#endif

#ifndef IRQF_SHARED
#define IRQF_SHARED SA_SHIRQ
#endif

#ifndef PCI_VDEVICE
#define PCI_VDEVICE(vendor, device) \
	PCI_VENDOR_ID_##vendor, (device), \
	PCI_ANY_ID, PCI_ANY_ID, 0, 0
#endif

#ifndef round_jiffies
#define round_jiffies(j) (j)
#endif

#ifndef netdev_tx_t
#define netdev_tx_t int
#endif

#ifndef __packed
#define __packed __attribute__ ((packed))
#endif

#ifndef RHEL_RELEASE_CODE
#define RHEL_RELEASE_CODE 0
#endif
#ifndef RHEL_RELEASE_VERSION
#define RHEL_RELEASE_VERSION(a, b) 0
#endif

/* Non-kernel version-specific definitions */
#ifndef IFLA_VF_PORT_MAX
#define PORT_PROFILE_MAX 40
#define PORT_UUID_MAX  16
#endif

#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3, 3, 0))
#include <net/flow_keys.h>
#else
#include <net/ip.h>
#include <stdbool.h>

struct flow_keys {
	__be32 src;
	__be32 dst;
	union {
		__be32 ports;
		__be16 port16[2];
	};
	u8 ip_proto;
};

#ifdef CONFIG_RFS_ACCEL
static inline bool skb_flow_dissect(const struct sk_buff *skb, struct flow_keys *flow)
{
	int nhoff;
	const struct iphdr *ip;
	const __be16 *ports;

	nhoff = skb_network_offset(skb);
	if (skb->protocol != htons(ETH_P_IP))
		return false;
	ip = (const struct iphdr *)(skb->data + nhoff);
	if (ip->frag_off & htons(IP_MF | IP_OFFSET))
		return false;
	ports = (const __be16 *)(skb->data + nhoff + 4 * ip->ihl);

	flow->src = ip->saddr;
	flow->dst = ip->daddr;
	flow->ip_proto = ip->protocol;
	flow->port16[0] = ports[0];
	flow->port16[1] = ports[1];
	return true;
}
#endif /*CONFIG_RFS_ACCEL*/
#endif /*LINUX >= 3.3.0*/

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3, 14, 0))
#define skb_get_hash_raw(skb) (skb)->rxhash
#endif

#if !defined(__VMKLNX__) && (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 24))
#define enic_wq_lock(wq_lock) spin_lock_irqsave(wq_lock, flags)
#define enic_wq_unlock(wq_lock) spin_unlock_irqrestore(wq_lock, flags)
#else
#define enic_wq_lock(wq_lock) spin_lock(wq_lock)
#define enic_wq_unlock(wq_lock) spin_unlock(wq_lock)
#endif /* ! vmklnx && kernel < 2.6.24 */

#ifdef CONFIG_RFS_ACCEL
#if ((RHEL_RELEASE_CODE && RHEL_RELEASE_CODE >= RHEL_RELEASE_VERSION(6, 5) \
			&& RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(7, 0)))
#define enic_netdev_rmap(enic) netdev_extended(enic->netdev)->rfs_data.rx_cpu_rmap
#else
#define enic_netdev_rmap(enic) enic->netdev->rx_cpu_rmap
#endif /*RHEL_RELEASE_VERSION >= 6.5 && < 7.0*/
#endif /*CONFIG_RFS_ACCEL*/

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3, 5, 00))
#define ether_addr_equal(i, j) (!(compare_ether_addr(i, j)))
#endif /* LINUX_VERSION_CODE < 3.5.0 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3, 9, 00))
#define NETIF_F_HW_VLAN_CTAG_RX NETIF_F_HW_VLAN_RX
#define NETIF_F_HW_VLAN_CTAG_TX NETIF_F_HW_VLAN_TX
#define enic_hlist_for_each_entry_safe(a, b, c, d, e) hlist_for_each_entry_safe(a, b, c, d, e)
#define enic_hlist_for_each_entry(a, b, c, d) hlist_for_each_entry(a, b, c, d)
#else
#define enic_hlist_for_each_entry_safe(a, b, c, d, e) hlist_for_each_entry_safe(a, c, d, e)
#define enic_hlist_for_each_entry(a, b, c, d) hlist_for_each_entry(a, c, d)
#endif /*KERNEL_VERSION > 3.9.0 */

#ifndef CONFIG_NET_RX_BUSY_POLL
#define napi_hash_del(napi) do {} while(0)
#define napi_hash_add(napi) do {} while(0)
#define skb_mark_napi_id(skb, napi) do {} while(0)
#endif /*CONFIG_NET_RX_BUSY_POLL*/

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3, 9, 00))
#define __vlan_hwaccel_put_tag(a, b, c) __vlan_hwaccel_put_tag(a, c);
#endif /* KERNEL < 3.9.0 */

#ifndef __VMKLNX__
#if ((LINUX_VERSION_CODE <= KERNEL_VERSION(3, 4, 0)) &&		\
     (!RHEL_RELEASE_CODE || RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 0)))
#define net_warn_ratelimited(fmt, ...)			\
	do {						\
		if (net_ratelimit())			\
			pr_warn(fmt, ##__VA_ARGS__);	\
	} while (0)
#endif /* kernel <= 3.4 && rhel < 6.0 */

#if (LINUX_VERSION_CODE <= KERNEL_VERSION(2, 6, 26))
#define enic_pci_dma_mapping_error(pdev, dma) pci_dma_mapping_error(dma)
#else
#define enic_pci_dma_mapping_error(pdev, dma) pci_dma_mapping_error(pdev, dma)
#endif /* Kernel version <= 2.6.26 */
#endif

/* Kernel version-specific definitions */
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 14))
static inline signed long schedule_timeout_uninterruptible(signed long timeout)
{
	set_current_state(TASK_UNINTERRUPTIBLE);
	return schedule_timeout(timeout);
}
#endif

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 15))
static inline void *kzalloc(size_t size, unsigned int flags)
{
	void *mem = kmalloc(size, flags);
	if (mem)
		memset(mem, 0, size);
	return mem;
}
#endif

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 18))
static inline int kcompat_skb_linearize(struct sk_buff *skb, int gfp)
{
	return skb_linearize(skb, gfp);
}
#undef skb_linearize
#define skb_linearize(skb) kcompat_skb_linearize(skb, GFP_ATOMIC)
#if !defined(__VMKLNX__)
#undef netdev_alloc_skb
#define netdev_alloc_skb(dev, len)	dev_alloc_skb(len)
#endif
#endif

#if !defined(__VMKLNX__)

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 19))
#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 2)))
typedef irqreturn_t (*irq_handler_t)(int, void*, struct pt_regs *);
#endif
typedef irqreturn_t (*kcompat_irq_handler_t)(int, void *);
static inline int kcompat_request_irq(unsigned int irq,
	kcompat_irq_handler_t handler, unsigned long flags,
	const char *devname, void *dev_id)
{
	irq_handler_t irq_handler = (irq_handler_t)handler;
	return request_irq(irq, irq_handler, flags, devname, dev_id);
}
#undef request_irq
#define request_irq(irq, handler, flags, devname, dev_id) \
	kcompat_request_irq(irq, handler, flags, devname, dev_id)
#endif

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 20))
#undef INIT_WORK
#define INIT_WORK(_work, _func) \
do { \
	INIT_LIST_HEAD(&(_work)->entry); \
	(_work)->pending = 0; \
	(_work)->func = (void (*)(void *))_func; \
	(_work)->data = _work; \
	init_timer(&(_work)->timer); \
} while (0)
#endif

#endif /* !defined(__VMKLNX__) */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 20))
#define csum_offset csum
#endif

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 22))

#define skb_checksum_start_offset(skb) skb_transport_offset(skb)
#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 4)))
#define ip_hdr(skb) (skb->nh.iph)
#define ipv6_hdr(skb) (skb->nh.ipv6h)
#define tcp_hdr(skb) (skb->h.th)
#define tcp_hdrlen(skb) (skb->h.th->doff << 2)
#define skb_transport_offset(skb) (skb->h.raw - skb->data)
#endif

#elif (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 38))
#if ((!RHEL_RELEASE_CODE) || \
    (RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 4)))
static inline int skb_checksum_start_offset(const struct sk_buff *skb)
{
	return skb->csum_start - skb_headroom(skb);
}
#endif /* RHEL < 6.4 */
#endif /* LINUX_VERSION_CODE < 2.6.22 */

#if defined(__VMKLNX__)

#define for_each_sg(start, var, count, index) \
		for ((var) = (start), (index) = 0; (index) < (count); \
			(var) = sg_next(var), (index)++)
#define kmem_cache kmem_cache_s
#define local_bh_disable() do { } while (0)
#define local_bh_enable() do { } while (0)

#else

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 23))
#undef kmem_cache_create
#define kmem_cache_create(name, size, align, flags, ctor) \
	kmem_cache_create(name, size, align, flags, ctor, NULL)
#define scsi_sglist(sc) ((struct scatterlist *) (sc)->request_buffer)
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 0)))
#define netif_wake_subqueue(dev, q) netif_wake_queue(dev)
#define netif_tx_wake_all_queues(dev) netif_wake_queue(dev)
#if ((RHEL_RELEASE_CODE == RHEL_RELEASE_VERSION(5, 6)) || \
	(RHEL_RELEASE_CODE == RHEL_RELEASE_VERSION(5, 7)))
#define alloc_etherdev_mq(n, tx) alloc_etherdev_mq(n, 1)
#else
#define alloc_etherdev_mq(n, tx) alloc_etherdev(n)
#endif
#endif /* RHEL < 6.0 */
#endif /* LINUX_VERSION_CODE < 2.6.24 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 24))
#define BIT(nr) (1UL << (nr))
#define for_each_sg(sglist, sg, nr, __i) \
	for (__i = 0, sg = (sglist); __i < (nr); __i++, sg++)
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 0)))
#define skb_get_queue_mapping(skb) 0
#endif
#endif /* LINUX_VERSION_CODE < 2.6.24 */

#endif /* defined(__VMKLNX__) */

#if ((LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 29)) && \
	(LINUX_VERSION_CODE >= KERNEL_VERSION(2, 6, 24))) || \
	defined(__VMKLNX__)

#if !defined(__VMKLNX__)
#define napi_complete(napi) netif_rx_complete((napi)->dev, napi)
#endif
#define napi_schedule_prep(napi) netif_rx_schedule_prep((napi)->dev, napi)
#define napi_schedule(napi) netif_rx_schedule((napi)->dev, napi)
#define __napi_schedule(napi) __netif_rx_schedule((napi)->dev, napi)
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 27)) && !defined(__VMKLNX__)
#define netif_napi_del(napi) do { } while (0)
#endif

#elif (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 24))

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 4)))
struct napi_struct {
	struct net_device *dev;
	int (*poll)(struct napi_struct *, int);
};
#define napi_complete(napi) netif_rx_complete((napi)->dev)
#endif
#define napi_schedule_prep(napi) netif_rx_schedule_prep((napi)->dev)
#define napi_schedule(napi) netif_rx_schedule((napi)->dev)
#define __napi_schedule(napi) __netif_rx_schedule((napi)->dev)
#define napi_enable(napi) netif_poll_enable((napi)->dev)
#define napi_disable(napi) netif_poll_disable((napi)->dev)
#define netif_napi_add(ndev, napi, _poll, _weight) \
	do { \
		struct napi_struct *__napi = napi; \
		ndev->poll = __enic_poll; \
		ndev->weight = _weight; \
		__napi->poll = _poll; \
		__napi->dev = ndev; \
		netif_poll_disable(ndev); \
	} while (0)
#define netif_napi_del(napi) do { } while (0)

#endif /* (2.6.24 <= LINUX_VERSION_CODE < 2.6.29) || __VMKLNX__ */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 25))

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 4)))
#define pci_enable_device_mem pci_enable_device
#endif

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 6)))
#define DEFINE_PCI_DEVICE_TABLE(_table) \
	const struct pci_device_id _table[]
#endif

#endif /* LINUX_VERSION_CODE < 2.6.25 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 27))

#define ethtool_cmd_speed_set(_ecmd, _speed)	\
	do {					\
		_ecmd->speed = _speed;		\
	} while (0)

#if !defined(__VMKLNX__)
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 0)))
#define netdev_get_tx_queue(dev, q) (dev)
#define netif_tx_stop_queue(dev) netif_stop_queue(dev)
#define netif_tx_queue_stopped(dev) netif_queue_stopped(dev)
#endif
#endif

#endif /* LINUX_VERSION_CODE < 2.6.27 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 28))

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 6)))
#undef pr_err
#define pr_err(fmt, ...) \
	printk(KERN_ERR pr_fmt(fmt), ##__VA_ARGS__)
#undef pr_warn
#define pr_warn pr_warning
#undef pr_warning
#define pr_warning(fmt, ...) \
	printk(KERN_WARNING pr_fmt(fmt), ##__VA_ARGS__)
#undef pr_info
#define pr_info(fmt, ...) \
	printk(KERN_INFO pr_fmt(fmt), ##__VA_ARGS__)
#endif

#endif /* LINUX_VERSION_CODE < 2.6.28 */

/*
 * We want this to be dependent on NETIF_F_GRO instead of kernel version,
 * because of past bugs we have seen.
 */
#ifndef NETIF_F_GRO
#define NETIF_F_GRO 0
#define vlan_gro_receive(napi, vlan_group, vlan_tci, skb) \
	vlan_hwaccel_receive_skb(skb, vlan_group, vlan_tci)
#define napi_gro_receive(napi, skb) \
	netif_receive_skb(skb)
#endif

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 33))
static inline struct sk_buff *kcompat_netdev_alloc_skb_ip_align(
	struct net_device *dev, unsigned int length)
{
	struct sk_buff *skb = netdev_alloc_skb(dev, length + NET_IP_ALIGN);

	if (NET_IP_ALIGN && skb)
		skb_reserve(skb, NET_IP_ALIGN);
	return skb;
}
#undef netdev_alloc_skb_ip_align
#define netdev_alloc_skb_ip_align(netdev, len) \
	kcompat_netdev_alloc_skb_ip_align(netdev, len)
#endif /* LINUX_VERSION_CODE < 2.6.33 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 34))

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 6)) || \
	(RHEL_RELEASE_CODE == RHEL_RELEASE_VERSION(6, 0)))
#define netdev_for_each_mc_addr(mclist, dev) \
	for (mclist = dev->mc_list; mclist; mclist = mclist->next)
#endif
#define netdev_mc_count(dev) ((dev)->mc_count)

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 6)))
static inline const char *netdev_name(const struct net_device *dev)
{
	if (dev->reg_state != NETREG_REGISTERED)
		return "(unregistered net_device)";
	return dev->name;
}
#endif

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 23))
#define netdev_uc_count(dev) 0
#elif (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 31))
#define netdev_for_each_uc_addr(uclist, dev) \
	for (uclist = dev->uc_list; uclist; uclist = uclist->next)
#define netdev_uc_count(dev) ((dev)->uc_count)
#elif (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 34))
#define netdev_for_each_uc_addr(ha, dev) \
	list_for_each_entry(ha, &dev->uc.list, list)
#define netdev_uc_count(dev) ((dev)->uc.count)
#endif

#if defined(__VMKLNX__)
#define netdev_printk(level, netdev, format, args...) \
	dev_printk(level, &((netdev)->pdev->dev), \
		"%s: " format, \
		netdev_name(netdev), ##args)
#elif (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 21))
#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 6)))
#define netdev_printk(level, netdev, format, args...) \
	dev_printk(level, (netdev)->class_dev.dev, \
		"%s: " format, \
		netdev_name(netdev), ##args)
#endif
#else
#define netdev_printk(level, netdev, format, args...) \
	dev_printk(level, (netdev)->dev.parent, \
		"%s: " format, \
		netdev_name(netdev), ##args)
#endif

#if ((!RHEL_RELEASE_CODE) || \
	(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(5, 6)))
#define netdev_err(dev, format, args...) \
	netdev_printk(KERN_ERR, dev, format, ##args)
#define netdev_warn(dev, format, args...) \
	netdev_printk(KERN_WARNING, dev, format, ##args)
#define netdev_info(dev, format, args...) \
	netdev_printk(KERN_INFO, dev, format, ##args)
#endif

/* To enable multi-wq capablity for netqueue from esxi5.0 onwards */
#if defined(ENIC_NETQ) 
#define netif_set_real_num_tx_queues(_dev, _txq)\
	do {					\
		(_dev)->real_num_tx_queues = _txq; \
	} while (0)	
#else
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 0)))
#if !defined(CONFIG_X86_XEN)
#define netif_set_real_num_tx_queues(_dev, _txq) do {} while (0)
#endif /*CONFIG_X86_XEN*/
#endif
#endif /* ENIC_NETQ */

#endif /* LINUX_VERSION_CODE < 2.6.34 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 37))
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 4)))
#define netif_set_real_num_rx_queues(_dev, _rxq) do {} while (0)
#endif
#endif /* LINUX_VERSION_CODE < 2.6.37 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 38))
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 2)))
#define alloc_etherdev_mqs(n, tx, rx) alloc_etherdev_mq(n, max_t(int, tx, rx))
#define call_netdevice_notifiers(_val, _dev) do {} while (0)
#endif
#endif /* LINUX_VERSION_CODE < 2.6.38 */

/* skb_record_rx_queue() required for netqueue from 
 * esxi5.0 onwards so we need to redfine it
 */
#if !defined(__VMKLNX__)
#if (LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 30))
#if ((!RHEL_RELEASE_CODE) || \
		(RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6, 0)))
#define skb_record_rx_queue(_skb, _rxq) do {} while (0)
#endif
#endif /* LINUX_VERSION_CODE < 2.6.30 */
#endif
#if defined(ENIC_NETQ)
#define skb_record_rx_queue(_skb, _rxq) \
		vmknetddi_queueops_set_skb_queueid((_skb), \
			VMKNETDDI_QUEUEOPS_MK_RX_QUEUEID((_rxq)))
#endif  /* ENIC_NETQ */    

#if (LINUX_VERSION_CODE <= KERNEL_VERSION(3, 1, 00))
#define skb_frag_dma_map(_dev, _frag, _offset, _size, _dir) \
	dma_map_page(_dev, _frag->page, _frag->page_offset + _offset, \
		_size, _dir)
#endif /* LINUX_VERSION_CODE <= 3.1.0 */

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3, 2, 00))
#define skb_frag_size(frag) frag->size
#endif /* LINUX_VERSION_CODE < 3.2.0 */

/* Don't forget about MIPS, include it last */
#ifdef CONFIG_MIPS
#include "kcompat_mips.h"
#endif

#else /* __KERNEL__ */


#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <linux/types.h>
#include <linux_types.h>
#include <kcompat_priv.h>
#include <assert.h>
#include <stdbool.h>

#define DIV_ROUND_UP(n, d) (((n) + (d) - 1) / (d))
#define ALIGN(x, a)              __ALIGN_MASK(x, (typeof(x))(a)-1)
#define __ALIGN_MASK(x, mask)    (((x)+(mask))&~(mask))
#define ETH_ALEN 6
#define BUG() assert(0)
#define BUG_ON(x) assert(!x)
#define kzalloc(x, flags) calloc(1, x)
#define kfree(x) free(x)

#define __iomem
#define udelay usleep
#define readl ioread32
#define writel iowrite32

static inline uint32_t ioread32(const volatile void *addr)
{
	return *(volatile uint32_t *)addr;
}

static inline uint16_t ioread16(const volatile void *addr)
{
	return *(volatile uint16_t *)addr;
}

static inline uint8_t ioread8(const volatile void *addr)
{
	return *(volatile uint8_t *)addr;
}

static inline void iowrite64(uint64_t val, const volatile void *addr)
{
	*(volatile uint64_t *)addr = val;
}

static inline void iowrite32(uint32_t val, const volatile void *addr)
{
	*(volatile uint32_t *)addr = val;
}

#endif /* __KERNEL__ */
#endif /* !__WINDOWS__ */
#endif /* _KCOMPAT_H_ */
