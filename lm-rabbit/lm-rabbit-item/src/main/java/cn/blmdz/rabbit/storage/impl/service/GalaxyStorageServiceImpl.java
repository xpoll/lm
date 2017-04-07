package cn.blmdz.rabbit.storage.impl.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.rabbit.storage.impl.manager.GalaxyStorageManager;
import cn.blmdz.wolf.item.impl.dao.SkuDao;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.storage.service.StorageService;
import lombok.extern.slf4j.Slf4j;

/**
 * Date: 6/28/16
 * Time: 11:05 PM
 * Author: 2016年 <a href="mailto:d@terminus.io">张成栋</a>
 */

@Slf4j
@Service
public class GalaxyStorageServiceImpl implements StorageService {
    private final SkuDao skuDao;
    private final GalaxyStorageManager galaxyStorageManager;

    @Autowired
    public GalaxyStorageServiceImpl(SkuDao skuDao, GalaxyStorageManager galaxyStorageManager) {
        this.skuDao = skuDao;
        this.galaxyStorageManager = galaxyStorageManager;
    }

    /**
     * 根据产品id(可以是skuId, 也可以是itemId, 甚至也可以是spuId等), 以及对应的仓库id, 查找对应的库存
     *
     * @param skuId   产品id
     * @param productType 产品类型, 决定skuId指sku, item还是spu
     * @param warehouseId 仓库id
     * @return 产品在对应仓库的库存
     */
    @Override
    public Response<Integer> findBy(Long skuId, Integer productType, Integer warehouseId) {
        throw new UnsupportedOperationException();
    }

    /**
     * 减少对应产品id (可以是skuId, 也可以是itemId, 甚至也可以是spuId等)及仓库id的库存
     *
     * @param skuId   产品id
     * @param productType 产品类型, 决定skuId指sku, item还是spu
     * @param warehouseId 仓库id
     * @param delta       库存变更数量
     * @return 是否变更成功
     */
    @Override
    public Response<Boolean> decreaseBy(Long skuId, Integer productType, Integer warehouseId, Integer delta) {
        try {
            galaxyStorageManager.decreaseBy(skuId, productType, warehouseId, delta);
            return Response.ok(Boolean.TRUE);
        } catch (ServiceException e) {
            log.error("fail to decrease by skuId:{},productType:{},warehouseId:{},delta:{},cause:{}",
                      skuId, productType, warehouseId, delta, Throwables.getStackTraceAsString(e));
            return Response.fail(e.getMessage());
        } catch (Exception e) {
            log.error("fail to decrease by skuId:{},productType:{},warehouseId:{},delta:{},cause:{}",
                      skuId, productType, warehouseId, delta, Throwables.getStackTraceAsString(e));
            return Response.fail("decrease.storage.fail");
        }
    }

    /**
     * 增加对应产品id (可以是skuId, 也可以是itemId, 甚至也可以是spuId等)及仓库id的库存
     *
     * @param productId   产品id
     * @param productType 产品类型, 决定skuId指sku, item还是spu
     * @param warehouseId 仓库id
     * @param delta       库存变更数量
     * @return 是否变更成功
     */
    @Override
    public Response<Boolean> increaseBy(Long productId, Integer productType, Integer warehouseId, Integer delta) {
        try {
            // 增加数量应该大于 0
            if (!Arguments.isPositive(delta)) {
                log.warn("fail to increase by skuId:{}, type:{}, warehouseId:{}, delta:{}, cause: delta must greater than 0",
                         productId, productType, warehouseId, delta);
                return Response.ok(Boolean.TRUE);
            }

            Sku sku = skuDao.findById(productId);
            if (sku == null) {
                log.error("sku(id={}) not found", productId);
                throw new ServiceException("sku.not.found");
            }
            final Long skuId = sku.getId();
            final Long itemId = sku.getItemId();
            galaxyStorageManager.increaseBy(skuId, itemId, delta);
            return Response.ok(Boolean.TRUE);
        } catch (Exception e) {
            log.error("fail to increase by skuId:{}, type:{}, warehouseId:{}, delta:{}, cause:{}",
                      productId, productType, warehouseId, delta, Throwables.getStackTraceAsString(e));
            return Response.fail("increase.storage.fail");
        }
    }

    /**
     * 设置对应产品id (可以是skuId, 也可以是itemId, 甚至也可以是spuId等)及仓库id的库存
     *
     * @param skuId   产品id
     * @param productType 产品类型, 决定skuId指sku, item还是spu
     * @param warehouseId 仓库id
     * @param quantity    库存目标数量
     * @return 是否设置成功
     */
    @Override
    public Response<Boolean> set(Long skuId, Integer productType, Integer warehouseId, Integer quantity) {
        throw new UnsupportedOperationException();
    }
}
