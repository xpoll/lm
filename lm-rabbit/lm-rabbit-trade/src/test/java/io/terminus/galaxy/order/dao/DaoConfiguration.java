/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package io.terminus.galaxy.order.dao;

import io.terminus.boot.dubbo.autoconfigure.DubboAutoConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * Author  : panxin
 * Date    : 3:46 PM 3/11/16
 * Mail    : panxin@terminus.io
 */
@Configuration
@EnableAutoConfiguration(exclude = DubboAutoConfiguration.class)
@ComponentScan({"io.terminus.galaxy.order.dao"})
public class DaoConfiguration {



}
