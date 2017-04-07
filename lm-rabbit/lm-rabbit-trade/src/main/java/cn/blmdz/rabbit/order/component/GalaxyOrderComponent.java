package cn.blmdz.rabbit.order.component;

import java.io.Serializable;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.rabbit.order.dao.OrderExtraDao;
import cn.blmdz.rabbit.order.dao.OrderFinishInfoDao;
import cn.blmdz.rabbit.order.dao.OrderMoneyFlowDao;
import cn.blmdz.rabbit.order.model.OrderExtra;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;
import cn.blmdz.rabbit.order.model.OrderMoneyFlow;
import cn.blmdz.wolf.order.component.OrderComponent;
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
import cn.blmdz.wolf.order.util.Params;

/**
 * Desc: 复写orderComponent中的方法
 * Mail: F@terminus.io
 * Data: 16/3/25
 * Author: yangzefeng
 */
public class GalaxyOrderComponent implements OrderComponent {

    private final MergeOrderDao mergeOrderDao;

    private final ShopOrderDao shopOrderDao;

    private final SkuOrderDao skuOrderDao;

    private final MergeOrderRefundDao mergeOrderRefundDao;

    private final ShopOrderRefundDao shopOrderRefundDao;

    private final SkuOrderRefundDao skuOrderRefundDao;

    private final OrderExtraDao orderExtraDao;

    private final OrderMoneyFlowDao orderMoneyFlowDao;

    private final OrderFinishInfoDao orderFinishInfoDao;

    @Autowired
    public GalaxyOrderComponent(MergeOrderDao mergeOrderDao,
                                ShopOrderDao shopOrderDao,
                                SkuOrderDao skuOrderDao,
                                MergeOrderRefundDao mergeOrderRefundDao,
                                ShopOrderRefundDao shopOrderRefundDao,
                                SkuOrderRefundDao skuOrderRefundDao,
                                OrderExtraDao orderExtraDao,
                                OrderMoneyFlowDao orderMoneyFlowDao,
                                OrderFinishInfoDao orderFinishInfoDao) {
        this.mergeOrderDao = mergeOrderDao;
        this.shopOrderDao = shopOrderDao;
        this.skuOrderDao = skuOrderDao;
        this.mergeOrderRefundDao = mergeOrderRefundDao;
        this.shopOrderRefundDao = shopOrderRefundDao;
        this.skuOrderRefundDao = skuOrderRefundDao;
        this.orderExtraDao = orderExtraDao;
        this.orderMoneyFlowDao = orderMoneyFlowDao;
        this.orderFinishInfoDao = orderFinishInfoDao;
    }

    /**
     * 检测订单的类型,然后选择具体的dao来创建订单实例
     * 这个方法可以被具体解决方案重写来拓展订单层级
     * @param order 待创建的订单,其中包含待创建的订单关联信息
     */
    @Override
    public void detectOrderTypeAndCreate(Order order) {
        if (order instanceof MergeOrder) {
            mergeOrderDao.create((MergeOrder) order);
        } else if (order instanceof ShopOrder) {
            shopOrderDao.create((ShopOrder) order);
        } else if (order instanceof SkuOrder) {
            skuOrderDao.create((SkuOrder) order);
        } else if (order instanceof MergeOrderRefund) {
            mergeOrderRefundDao.create((MergeOrderRefund) order);
        } else if (order instanceof ShopOrderRefund) {
            shopOrderRefundDao.create((ShopOrderRefund) order);
        } else if (order instanceof SkuOrderRefund) {
            skuOrderRefundDao.create((SkuOrderRefund) order);
        } else {
            throw new ServiceException("unknown.order.type");
        }
    }

    /**
     * 检测订单的类型,然后选择具体的dao来更新订单实例
     * 这个方法可以被具体解决方案重写来拓展订单层级
     * @param order 待更新的订单
     */
    @Override
    public void detectOrderTypeAndUpdate(Order order) {
        if (order instanceof MergeOrder) {
            mergeOrderDao.update((MergeOrder) order);
        } else if (order instanceof ShopOrder) {
            shopOrderDao.update((ShopOrder) order);
        } else if (order instanceof SkuOrder) {
            skuOrderDao.update((SkuOrder) order);
        } else if (order instanceof MergeOrderRefund) {
            mergeOrderRefundDao.update((MergeOrderRefund) order);
        } else if (order instanceof ShopOrderRefund) {
            shopOrderRefundDao.update((ShopOrderRefund) order);
        } else if (order instanceof SkuOrderRefund) {
            skuOrderRefundDao.update((SkuOrderRefund) order);
        } else {
            throw new ServiceException("unknown.order.type");
        }
    }

    /**
     * 创建和订单相关的一些对象,和创建订单在同一事务中
     * @param context 订单相关对象,例如orderExtra都会放在上下文中
     */
    @Override
    public void createOrderExtra(Map<String, Serializable> context, Order order) {
        if (null == context) {
            return;
        }
        OrderExtra orderExtra = (OrderExtra) Params.get(context, "orderExtra");
        if (null != orderExtra) {
            orderExtra.setOrderId(order.getId());
            orderExtraDao.create(orderExtra);
        }
    }

    /**
     * 更新和订单相关的一些对象,和更新订单在同一事务中
     * @param context 订单相关对象,例如orderExtra都会放在上下文中
     */
    @Override
    public void updateOrderExtra(Map<String, Serializable> context) {
        if (null == context) {
            return;
        }
        OrderExtra orderExtra = (OrderExtra) Params.get(context, "orderExtra");
        if (null != orderExtra) {
            orderExtraDao.update(orderExtra);
        }
        OrderMoneyFlow flow = (OrderMoneyFlow) Params.get(context, "orderMoneyFlow");
        if (null != flow) {
            orderMoneyFlowDao.create(flow);
        }

        OrderFinishInfo info = (OrderFinishInfo) Params.get(context, "orderFinishInfo");
        if (null != info) {
            orderFinishInfoDao.create(info);
        }
    }

    /**
     * 检测订单的类型,然后选择具体的dao来查询订单实例
     * 这个方法可以被具体解决方案重写来拓展订单层级
     * @param order 待更新的订单
     */
    @Override
    public Order detectOrderTypeAndQuery(Order order) {
        if (order instanceof MergeOrder) {
            return mergeOrderDao.findById(order.getId());
        } else if (order instanceof ShopOrder) {
            return shopOrderDao.findById(order.getId());
        } else if (order instanceof SkuOrder) {
            return skuOrderDao.findById(order.getId());
        } else if (order instanceof MergeOrderRefund) {
            return mergeOrderRefundDao.findById(order.getId());
        } else if (order instanceof ShopOrderRefund) {
            return shopOrderRefundDao.findById(order.getId());
        } else if (order instanceof SkuOrderRefund) {
            return skuOrderRefundDao.findById(order.getId());
        } else {
            throw new ServiceException("unknown.order.type");
        }
    }
}
