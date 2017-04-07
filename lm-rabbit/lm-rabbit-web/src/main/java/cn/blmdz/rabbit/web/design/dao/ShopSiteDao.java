package cn.blmdz.rabbit.web.design.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.redis.utils.JedisTemplate;

/**
 * Created by IntelliJ IDEA.
 * User: AnsonChan
 * Date: 15/1/13
 */
@Repository
public class ShopSiteDao {
    private JedisTemplate jedisTemplate;

    @Autowired
    public ShopSiteDao(@Qualifier("pampasJedisTemplate") JedisTemplate jedisTemplate) {
        this.jedisTemplate = jedisTemplate;
    }
}
