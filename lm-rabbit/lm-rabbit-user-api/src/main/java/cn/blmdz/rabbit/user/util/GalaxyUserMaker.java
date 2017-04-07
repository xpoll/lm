/*
 * Copyright (c) 2016. 杭州端点网络科技有限公司.  All rights reserved.
 */

package cn.blmdz.rabbit.user.util;

import com.google.common.base.Strings;

import cn.blmdz.home.common.util.BeanMapper;
import cn.blmdz.wolf.common.model.ParanaUser;
import cn.blmdz.wolf.user.model.User;

/**
 * Author:  <a href="mailto:i@terminus.io">jlchen</a>
 * Date: 2016-01-31
 */
public abstract class GalaxyUserMaker {
    public static ParanaUser from(User user){
        ParanaUser paranaUser = new ParanaUser();
        BeanMapper.copy(user, paranaUser);
        paranaUser.setName(getUserName(user));
        return paranaUser;
    }

    private static String getUserName(User user) {
        if (!Strings.isNullOrEmpty(user.getName())) {
            return user.getName();
        }
        if (!Strings.isNullOrEmpty(user.getEmail())) {
            return user.getEmail();
        }
        if (!Strings.isNullOrEmpty(user.getMobile())) {
            return user.getMobile();
        }
        return "";
    }
}
