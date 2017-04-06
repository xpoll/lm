package cn.blmdz.wolf.order.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.order.dao.MergeOrderDao;
import cn.blmdz.wolf.order.dao.MergeOrderRefundDao;
import cn.blmdz.wolf.order.dao.ShopOrderDao;
import cn.blmdz.wolf.order.dao.ShopOrderRefundDao;
import cn.blmdz.wolf.order.dao.SkuOrderDao;
import cn.blmdz.wolf.order.dao.SkuOrderRefundDao;
import cn.blmdz.wolf.order.model.MergeOrder;
import cn.blmdz.wolf.order.model.MergeOrderRefund;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.ShopOrderRefund;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;
import cn.blmdz.wolf.order.service.OrderReadService;
import cn.blmdz.wolf.order.utils.CriteriaBuilder;

@Service
public class OrderReadServiceImpl implements OrderReadService {
   private static final Logger log = LoggerFactory.getLogger(OrderReadServiceImpl.class);
   private final MergeOrderDao mergeOrderDao;
   private final ShopOrderDao shopOrderDao;
   private final SkuOrderDao skuOrderDao;
   private final MergeOrderRefundDao mergeOrderRefundDao;
   private final ShopOrderRefundDao shopOrderRefundDao;
   private final SkuOrderRefundDao skuOrderRefundDao;

   @Autowired
   public OrderReadServiceImpl(MergeOrderDao mergeOrderDao, ShopOrderDao shopOrderDao, SkuOrderDao skuOrderDao, MergeOrderRefundDao mergeOrderRefundDao, ShopOrderRefundDao shopOrderRefundDao, SkuOrderRefundDao skuOrderRefundDao) {
      this.mergeOrderDao = mergeOrderDao;
      this.shopOrderDao = shopOrderDao;
      this.skuOrderDao = skuOrderDao;
      this.mergeOrderRefundDao = mergeOrderRefundDao;
      this.shopOrderRefundDao = shopOrderRefundDao;
      this.skuOrderRefundDao = skuOrderRefundDao;
   }

   public Response findMergeOrderById(Long id) {
      try {
         MergeOrder mergeOrder = (MergeOrder)this.mergeOrderDao.findById(id);
         if(null == mergeOrder) {
            log.error("merge order id = {} not found", id);
            return Response.fail("order.not.found");
         } else {
            return Response.ok(mergeOrder);
         }
      } catch (Exception var3) {
         log.error("fail to find merge order by id {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.query.fail");
      }
   }

   public Response findMergeOrderRefundById(Long id) {
      try {
         MergeOrderRefund mergeOrderRefund = (MergeOrderRefund)this.mergeOrderRefundDao.findById(id);
         if(null == mergeOrderRefund) {
            log.error("merge order refund id = {} not found", id);
            return Response.fail("merge.order.refund.query.fail");
         } else {
            return Response.ok(mergeOrderRefund);
         }
      } catch (Exception var3) {
         log.error("fail to find merge order refund by id {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("merge.order.refund.query.fail");
      }
   }

   public Response findShopOrderById(Long id) {
      try {
         ShopOrder shopOrder = (ShopOrder)this.shopOrderDao.findById(id);
         if(null == shopOrder) {
            log.error("shop order id = {} not found", id);
            return Response.fail("order.not.found");
         } else {
            return Response.ok(shopOrder);
         }
      } catch (Exception var3) {
         log.error("fail to find shop order by id {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.query.fail");
      }
   }

   public Response findSkuOrderById(Long id) {
      try {
         SkuOrder skuOrder = (SkuOrder)this.skuOrderDao.findById(id);
         if(null == skuOrder) {
            log.error("sku order id = {} not found", id);
            return Response.fail("order.not.found");
         } else {
            return Response.ok(skuOrder);
         }
      } catch (Exception var3) {
         log.error("fail to find sku order by id {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.query.fail");
      }
   }

   public Response findMergeOrderByIds(List ids) {
      if(CollectionUtils.isEmpty(ids)) {
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<MergeOrder> mergeOrders = this.mergeOrderDao.findByIds(ids);
            return Response.ok(mergeOrders);
         } catch (Exception var3) {
            log.error("fail to find merge order by ids {}, cause:{}", ids, Throwables.getStackTraceAsString(var3));
            return Response.fail("order.query.fail");
         }
      }
   }

   public Response findShopOrderByIds(List ids) {
      if(CollectionUtils.isEmpty(ids)) {
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<ShopOrder> shopOrders = this.shopOrderDao.findByIds(ids);
            return Response.ok(shopOrders);
         } catch (Exception var3) {
            log.error("fail to find shop order by ids {}, cause:{}", ids, Throwables.getStackTraceAsString(var3));
            return Response.fail("order.query.fail");
         }
      }
   }

   public Response findSkuOrderByIds(List ids) {
      if(CollectionUtils.isEmpty(ids)) {
         return Response.ok(Collections.emptyList());
      } else {
         try {
            List<SkuOrder> skuOrders = this.skuOrderDao.findByIds(ids);
            return Response.ok(skuOrders);
         } catch (Exception var3) {
            log.error("fail to find sku order by ids {}, cause:{}", ids, Throwables.getStackTraceAsString(var3));
            return Response.fail("order.query.fail");
         }
      }
   }

   public Response findMergeOrderBy(Long buyerId, String nids, List orderIds, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         Map<String, Object> criteria = (new CriteriaBuilder.Builder()).buyerId(buyerId).nids(nids).orderIds(orderIds).startAt(startAt).endAt(endAt).pageInfo(pageNo, size).build().toMap();
         Paging<MergeOrder> mergeOrderP = this.mergeOrderDao.paging(criteria);
         return Response.ok(mergeOrderP);
      } catch (Exception var10) {
         log.error("fail to paging merge order by buyerId {}, nid {}, orderIds {}, startAt {}, endAt {}, pageNo {}, size {}, cause:{}", new Object[]{buyerId, nids, orderIds, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var10)});
         return Response.fail("order.query.fail");
      }
   }

   public Response findShopOrderBy(Long buyerId, Long shopId, String nids, List orderIds, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         Map<String, Object> criteria = (new CriteriaBuilder.Builder()).buyerId(buyerId).shopId(shopId).nids(nids).orderIds(orderIds).startAt(startAt).endAt(endAt).pageInfo(pageNo, size).build().toMap();
         Paging<ShopOrder> shopOrderP = this.shopOrderDao.paging(criteria);
         return Response.ok(shopOrderP);
      } catch (Exception var11) {
         log.error("fail to paging shop order by buyerId {}, shopId {}, nid {}, orderIds {}, startAt {}, endAt {}, pageNo {}, size {}, cause:{}", new Object[]{buyerId, shopId, nids, orderIds, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var11)});
         return Response.fail("order.query.fail");
      }
   }

   public Response findSkuOrderBy(Long buyerId, Long shopId, String nids, List orderIds, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         Map<String, Object> criteria = (new CriteriaBuilder.Builder()).buyerId(buyerId).shopId(shopId).nids(nids).orderIds(orderIds).startAt(startAt).endAt(endAt).pageInfo(pageNo, size).build().toMap();
         Paging<SkuOrder> skuOrderP = this.skuOrderDao.paging(criteria);
         return Response.ok(skuOrderP);
      } catch (Exception var11) {
         log.error("fail to paging sku order by buyerId {}, shopId {}, nid {}, orderIds {}, startAt {}, endAt {}, pageNo {}, size {}, cause:{}", new Object[]{buyerId, shopId, nids, orderIds, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var11)});
         return Response.fail("order.query.fail");
      }
   }

   public Response findSkuOrderRefundBy(Long buyerId, Long shopId, List parentIds, String nids, List orderIds, String startAt, String endAt, Integer pageNo, Integer size) {
      try {
         Map<String, Object> criteria = (new CriteriaBuilder.Builder()).buyerId(buyerId).shopId(shopId).nids(nids).orderIds(orderIds).parentIds(parentIds).startAt(startAt).endAt(endAt).pageInfo(pageNo, size).build().toMap();
         Paging<SkuOrderRefund> skuOrderRefundP = this.skuOrderRefundDao.paging(criteria);
         return Response.ok(skuOrderRefundP);
      } catch (Exception var12) {
         log.error("fail to find sku order refund by buyerId {}, shopId {}, nids {}, orderIds {}, startAt {}, endAt {}, pageNo {}, size {}", new Object[]{buyerId, shopId, nids, orderIds, startAt, endAt, pageNo, size, Throwables.getStackTraceAsString(var12)});
         return Response.fail("sku.order.refund.query.fail");
      }
   }

   public Response findShopOrderByParentId(Long mergeOrderId) {
      try {
         List<ShopOrder> shopOrders = this.shopOrderDao.findByParentId(mergeOrderId);
         return CollectionUtils.isEmpty(shopOrders)?Response.fail("shop.order.query.fail"):Response.ok(shopOrders);
      } catch (Exception var3) {
         log.error("fail to find shop order by merge order id {}, cause:{}", mergeOrderId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.order.query.fail");
      }
   }

   public Response findSkuOrderByParentId(Long shopOrderId) {
      try {
         List<SkuOrder> skuOrders = this.skuOrderDao.findByParentId(shopOrderId);
         return CollectionUtils.isEmpty(skuOrders)?Response.fail("sku.order.query.fail"):Response.ok(skuOrders);
      } catch (Exception var3) {
         log.error("fail to find sku order by shop order id {}, cause:{}", shopOrderId, Throwables.getStackTraceAsString(var3));
         return Response.fail("sku.order.query.fail");
      }
   }

   public Response findShopOrderByParentIds(List mergeOrderIds) {
      try {
         List<ShopOrder> shopOrders = this.shopOrderDao.findByParentIds(mergeOrderIds);
         return CollectionUtils.isEmpty(shopOrders)?Response.fail("shop.order.query.fail"):Response.ok(shopOrders);
      } catch (Exception var3) {
         log.error("fail to find shop order by parent ids {}, cause:{}", mergeOrderIds, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.order.query.fail");
      }
   }

   public Response findSkuOrderByParentIds(List shopOrderIds) {
      try {
         if(CollectionUtils.isEmpty(shopOrderIds)) {
            return Response.ok(Collections.emptyList());
         } else {
            List<SkuOrder> skuOrders = this.skuOrderDao.findByParentIds(shopOrderIds);
            return Response.ok(skuOrders);
         }
      } catch (Exception var3) {
         log.error("fail to find sku order by shop order ids {}, cause:{}", shopOrderIds, Throwables.getStackTraceAsString(var3));
         return Response.fail("order.query.fail");
      }
   }

   public Response findSkuOrderRefundById(Long id) {
      try {
         SkuOrderRefund skuOrderRefund = (SkuOrderRefund)this.skuOrderRefundDao.findById(id);
         if(null == skuOrderRefund) {
            log.error("sku order refund id={} not found", id);
            return Response.fail("sku.order.refund.not.found");
         } else {
            return Response.ok(skuOrderRefund);
         }
      } catch (Exception var3) {
         log.error("fail to find sku order refund by id {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("sku.order.refund.query.fail");
      }
   }

   public Response findSkuOrderRefundByParentId(Long parentId) {
      try {
         List<SkuOrderRefund> skuOrderRefunds = this.skuOrderRefundDao.findByParentId(parentId);
         return CollectionUtils.isEmpty(skuOrderRefunds)?Response.fail("sku.order.refund.query.fail"):Response.ok(skuOrderRefunds);
      } catch (Exception var3) {
         log.error("fail to find sku order refund by parent id {}, cause:{}", parentId, Throwables.getStackTraceAsString(var3));
         return Response.fail("sku.order.refund.query.fail");
      }
   }

   public Response findSkuOrderRefundByParentIds(List skuOrderIds) {
      try {
         if(CollectionUtils.isEmpty(skuOrderIds)) {
            return Response.ok(Collections.emptyList());
         } else {
            List<SkuOrderRefund> skuOrderRefunds = this.skuOrderRefundDao.findByParentIds(skuOrderIds);
            return Response.ok(skuOrderRefunds);
         }
      } catch (Exception var3) {
         log.error("fail to find sku order refund by parent ids {}, cause:{}", skuOrderIds, Throwables.getStackTraceAsString(var3));
         return Response.fail("sku.order.refund.query.fail");
      }
   }

   public Response findShopOrderRefundById(Long id) {
      try {
         ShopOrderRefund shopOrderRefund = (ShopOrderRefund)this.shopOrderRefundDao.findById(id);
         return null == shopOrderRefund?Response.fail("shop.order.refund.query.fail"):Response.ok(shopOrderRefund);
      } catch (Exception var3) {
         log.error("fail to find shop order refund by id {}, cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.order.refund.query.fail");
      }
   }

   public Response findShopOrderRefundByParentId(Long parentId) {
      try {
         List<ShopOrderRefund> shopOrderRefunds = this.shopOrderRefundDao.findByParentId(parentId);
         return CollectionUtils.isEmpty(shopOrderRefunds)?Response.fail("shop.order.refund.query.fail"):Response.ok(shopOrderRefunds);
      } catch (Exception var3) {
         log.error("fail to find shop order refund by parent id {}, cause:{}", parentId, Throwables.getStackTraceAsString(var3));
         return Response.fail("shop.order.refund.query.fail");
      }
   }
}
