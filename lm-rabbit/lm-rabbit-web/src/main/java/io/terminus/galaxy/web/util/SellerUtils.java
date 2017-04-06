package io.terminus.galaxy.web.util;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.pampas.common.UserUtil;
import io.terminus.parana.common.model.ParanaUser;

/**
 * @author Effet
 */
public class SellerUtils {

    public static Long getLoggedShopId() {
        return getLoggedShopId(UserUtil.<ParanaUser>getCurrentUser());
    }

    public static Long getLoggedShopId(ParanaUser user) {
        if (user == null) {
            throw new JsonResponseException(401, "user.not.login");
        }
        /*
        if (!Objects.equals(user.getType(), UserType.NORMAL.value())) {
            throw new JsonResponseException(403, "user.no.permission");
        }
        */
        if (user.getShopId() == null) {
            throw new JsonResponseException(403, "shop.not.exist");
        }
        return user.getShopId();
    }
}
