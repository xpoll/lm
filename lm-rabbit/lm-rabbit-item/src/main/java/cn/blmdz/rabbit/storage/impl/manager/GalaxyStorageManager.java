package cn.blmdz.rabbit.storage.impl.manager;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Objects;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.wolf.item.impl.dao.ItemDao;
import cn.blmdz.wolf.item.impl.dao.SkuDao;
import cn.blmdz.wolf.parana.item.model.Item;
import cn.blmdz.wolf.parana.item.model.Sku;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 6/28/16
 * Time: 11:09 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */
@Slf4j
@Component
public class GalaxyStorageManager {

    private final ItemDao itemDao;

    private final SkuDao skuDao;

    @Autowired
    public GalaxyStorageManager(ItemDao itemDao, SkuDao skuDao) {
        this.itemDao = itemDao;
        this.skuDao = skuDao;
    }

    /**
     * 减少对应产品id (可以是skuId, 也可以是itemId, 甚至也可以是spuId等)及仓库id的库存
     *
     * @param productId   产品id (可以是skuId, 也可以是itemId, 甚至也可以是spuId等)
     * @param productType 产品类型, 决定productId指sku, item还是spu
     * @param warehouseId 仓库id
     * @param delta       库存变更数量
     */
    @Transactional
    public void decreaseBy(Long productId, Integer productType, Integer warehouseId, Integer delta) {
        //TODO 考虑productType和warehouseId
        Sku sku = skuDao.findById(productId);
        if (sku == null) {
            log.error("sku(id={}) not found", productId);
            throw new ServiceException("sku.not.found");
        }
        final Long skuId = sku.getId();

        final Long itemId = sku.getItemId();
        Item item = itemDao.findById(itemId);

        //检查是否有库存
        checkStockIfEnough(item, sku, delta);

        skuDao.updateStockQuantity(skuId, delta);
        itemDao.updateSaleQuantity(itemId, delta);

        //如果商品处于上架状态则检查是否需要下架该商品
        if (Objects.equal(item.getStatus(), 1)) {
            if (item.getStockQuantity() - delta <= 0) {
                itemDao.updateStatus(itemId, -1);
            }
        }
        if (Objects.equal(sku.getStatus(), 1)) {
            if (sku.getStockQuantity() - delta <= 0) {
                skuDao.updateStatusBySkuId(skuId, -1);
            }
        }
    }

    private void checkStockIfEnough(Item item, Sku sku, Integer delta) {
        if (item.getStockQuantity() - delta < 0) {
            log.error("stock quantity not enough where item id={},expect quantity:{},but actual quantity:{}",
                      item.getId(), delta, item.getStockQuantity());
            throw new ServiceException("item.stock.quantity.not.enough");
        }
        if (sku.getStockQuantity() - delta < 0) {
            log.error("stock quantity not enough where sku id={},expect quantity:{},but actual quantity:{}",
                      sku.getId(), delta, sku.getStockQuantity());
            throw new ServiceException("sku.stock.quantity.not.enough");
        }
    }

    /**
     * 增加对应产品id
     *
     * @param skuId   sku id
     * @param itemId   商品 id
     * @param delta       库存变更数量
     */
    @Transactional
    public void increaseBy(Long skuId, Long itemId, Integer delta) {
        // update stock quantity 只能减去库存,这里用负数增加库存
        skuDao.updateStockQuantity(skuId, -delta);
        itemDao.updateSaleQuantity(itemId, -delta);
    }
}
