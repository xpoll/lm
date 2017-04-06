package io.terminus.galaxy.web.core.trade.util;

import io.terminus.common.utils.JsonMapper;
import io.terminus.parana.order.dto.UserTypeAndAction;
import io.terminus.parana.order.model.OrderActionInstance;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Desc: 一些数据结构转json的工具类
 * Mail: F@terminus.io
 * Data: 16/3/9
 * Author: yangzefeng
 */
public class ParseJson {

    private final static JsonMapper JSON_MAPPER = JsonMapper.nonDefaultMapper();

    public static String getNextActionsJson(Map<Integer, List<OrderActionInstance>> userTypeAndActionMap) {
        List<UserTypeAndAction> userTypeAndActions = new ArrayList<>();
        for (Integer userType : userTypeAndActionMap.keySet()) {
            UserTypeAndAction userTypeAndAction = new UserTypeAndAction();
            userTypeAndAction.setType(userType);
            List<OrderActionInstance> actionInstances = userTypeAndActionMap.get(userType);
            List<Long> actionInstanceIds = new ArrayList<>();
            for (OrderActionInstance orderActionInstance : actionInstances) {
                actionInstanceIds.add(orderActionInstance.getId());
            }
            userTypeAndAction.setActionInstanceIds(actionInstanceIds);

            userTypeAndActions.add(userTypeAndAction);
        }
        return JSON_MAPPER.toJson(userTypeAndActions);
    }

    public static List<UserTypeAndAction> parseToUserTypeAndActions(String json) {
        return JSON_MAPPER.fromJson(json, JSON_MAPPER.createCollectionType(List.class, UserTypeAndAction.class));
    }

    public static Map<String, String> parseToStringStringMap(String json) {
        return JSON_MAPPER.fromJson(json, JSON_MAPPER.createCollectionType(Map.class, String.class, String.class));
    }
}
