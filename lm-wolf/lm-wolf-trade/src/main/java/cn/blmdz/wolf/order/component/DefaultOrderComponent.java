package cn.blmdz.wolf.order.component;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.order.dao.MergeOrderDao;
import cn.blmdz.wolf.order.dao.MergeOrderRefundDao;
import cn.blmdz.wolf.order.dao.ShopOrderDao;
import cn.blmdz.wolf.order.dao.ShopOrderRefundDao;
import cn.blmdz.wolf.order.dao.SkuOrderDao;
import cn.blmdz.wolf.order.dao.SkuOrderRefundDao;
import cn.blmdz.wolf.order.model.MergeOrder;
import cn.blmdz.wolf.order.model.MergeOrderRefund;
import cn.blmdz.wolf.order.model.Order;
import cn.blmdz.wolf.order.model.ShopOrder;
import cn.blmdz.wolf.order.model.ShopOrderRefund;
import cn.blmdz.wolf.order.model.SkuOrder;
import cn.blmdz.wolf.order.model.SkuOrderRefund;

public class DefaultOrderComponent implements OrderComponent {
   private final MergeOrderDao mergeOrderDao;
   private final ShopOrderDao shopOrderDao;
   private final SkuOrderDao skuOrderDao;
   private final MergeOrderRefundDao mergeOrderRefundDao;
   private final ShopOrderRefundDao shopOrderRefundDao;
   private final SkuOrderRefundDao skuOrderRefundDao;

   @Autowired
   public DefaultOrderComponent(MergeOrderDao mergeOrderDao, ShopOrderDao shopOrderDao, SkuOrderDao skuOrderDao, MergeOrderRefundDao mergeOrderRefundDao, ShopOrderRefundDao shopOrderRefundDao, SkuOrderRefundDao skuOrderRefundDao) {
      this.mergeOrderDao = mergeOrderDao;
      this.shopOrderDao = shopOrderDao;
      this.skuOrderDao = skuOrderDao;
      this.mergeOrderRefundDao = mergeOrderRefundDao;
      this.shopOrderRefundDao = shopOrderRefundDao;
      this.skuOrderRefundDao = skuOrderRefundDao;
   }

   public void detectOrderTypeAndCreate(Order order) {
      if(order instanceof MergeOrder) {
         this.mergeOrderDao.create((MergeOrder)order);
      } else if(order instanceof ShopOrder) {
         this.shopOrderDao.create((ShopOrder)order);
      } else if(order instanceof SkuOrder) {
         this.skuOrderDao.create((SkuOrder)order);
      } else if(order instanceof MergeOrderRefund) {
         this.mergeOrderRefundDao.create((MergeOrderRefund)order);
      } else if(order instanceof ShopOrderRefund) {
         this.shopOrderRefundDao.create((ShopOrderRefund)order);
      } else {
         if(!(order instanceof SkuOrderRefund)) {
            throw new ServiceException("unknown.order.type");
         }

         this.skuOrderRefundDao.create((SkuOrderRefund)order);
      }

   }

   public void createOrderExtra(Map context, Order order) {
   }

   public void detectOrderTypeAndUpdate(Order order) {
      if(order instanceof MergeOrder) {
         this.mergeOrderDao.update((MergeOrder)order);
      } else if(order instanceof ShopOrder) {
         this.shopOrderDao.update((ShopOrder)order);
      } else if(order instanceof SkuOrder) {
         this.skuOrderDao.update((SkuOrder)order);
      } else if(order instanceof MergeOrderRefund) {
         this.mergeOrderRefundDao.update((MergeOrderRefund)order);
      } else if(order instanceof ShopOrderRefund) {
         this.shopOrderRefundDao.update((ShopOrderRefund)order);
      } else {
         if(!(order instanceof SkuOrderRefund)) {
            throw new ServiceException("unknown.order.type");
         }

         this.skuOrderRefundDao.update((SkuOrderRefund)order);
      }

   }

   public void updateOrderExtra(Map context) {
   }

   public Order detectOrderTypeAndQuery(Order order) {
      if(order instanceof MergeOrder) {
         return (Order)this.mergeOrderDao.findById(order.getId());
      } else if(order instanceof ShopOrder) {
         return (Order)this.shopOrderDao.findById(order.getId());
      } else if(order instanceof SkuOrder) {
         return (Order)this.skuOrderDao.findById(order.getId());
      } else if(order instanceof MergeOrderRefund) {
         return (Order)this.mergeOrderRefundDao.findById(order.getId());
      } else if(order instanceof ShopOrderRefund) {
         return (Order)this.shopOrderRefundDao.findById(order.getId());
      } else if(order instanceof SkuOrderRefund) {
         return (Order)this.skuOrderRefundDao.findById(order.getId());
      } else {
         throw new ServiceException("unknown.order.type");
      }
   }
}
