package io.terminus.galaxy.common.util;

import io.terminus.common.model.BaseUser;
import io.terminus.galaxy.common.enums.UserRole;
import io.terminus.galaxy.common.enums.UserType;

import java.util.List;

/**
 * 用户角色工具类
 *
 * @author Effet
 */
public class UserRoleUtil {

    /**
     * 判断用户是否为 ADMIN
     *
     * @param user 用户信息
     * @return 是否为 ADMIN
     */
    public static boolean isAdmin(BaseUser user) {
        return user != null && isAdmin(user.getType());
    }

    public static boolean isAdmin(Integer type) {
        return type != null && type == UserType.ADMIN.value();
    }

    public static boolean isOperator(BaseUser user) {
        return isOperator(user.getType());
    }

    public static boolean isOperator(Integer type) {
        return type != null && type == UserType.OPERATOR.value();
    }

    public static boolean isNormal(BaseUser user) {
        return isNormal(user.getType());
    }

    public static boolean isNormal(Integer type) {
        return type != null && type == UserType.NORMAL.value();
    }

    public static boolean isSeller(BaseUser user) {
        return isSeller(user.getType(), user.getRoles());
    }

    public static boolean isSeller(Integer type, List<String> roles) {
        return isNormal(type) && roles != null && roles.contains(UserRole.SELLER.name());
    }

    public static boolean isBuyer(BaseUser user) {
        return isBuyer(user.getType(), user.getRoles());
    }

    public static boolean isBuyer(Integer type, List<String> roles) {
        return isNormal(type) && roles != null && roles.contains(UserRole.BUYER.name());
    }
}
