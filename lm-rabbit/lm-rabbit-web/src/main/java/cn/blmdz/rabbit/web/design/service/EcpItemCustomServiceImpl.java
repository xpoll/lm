/*
 * <!--
 *   ~ Copyright (c) 2014 杭州端点网络科技有限公司
 *   -->
 */

package cn.blmdz.rabbit.web.design.service;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import cn.blmdz.hunt.design.dao.ItemCustomRedisDao;
import cn.blmdz.hunt.design.service.ItemCustomService;

/**
 * Created by yangzefeng on 14/11/19
 */
@Service
public class EcpItemCustomServiceImpl implements EcpItemCustomService {

    @Autowired(required = false)
    private ItemCustomRedisDao itemCustomRedisDao;

    @Autowired(required = false)
    private ItemCustomService itemCustomService;

    @Override
    public String getRenderHbs(Long itemId, Long spuId, Map<String, String> context) {

        return itemCustomRedisDao.findById(itemId);
    }

    @Override
    public String renderTemplate(Long spuId, Map<String, String> context) {
        return itemCustomService.renderTemplate(spuId, context);
    }

    @Override
    public String render(Long itemId, Long spuId, Map<String, String> context) {
        return itemCustomService.render(itemId, spuId, context);
    }
}
