/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.user.impl.dao;

import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import cn.blmdz.boot.dubbo.autoconfigure.DubboAutoConfiguration;
import cn.blmdz.boot.mybatis.autoconfigure.MybatisAutoConfiguration;

/**
 * 用户模块DAO层测试
 *
 *  updated by panxin@terminus.io
 */
@Configuration
@EnableAutoConfiguration(exclude = DubboAutoConfiguration.class)
@ComponentScan({"io.terminus.galaxy.user.impl.dao"})
@AutoConfigureAfter(MybatisAutoConfiguration.class)
public class DaoConfiguration {
}
