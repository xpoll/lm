package cn.blmdz.rabbit.order.dao;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Repository;

import com.google.common.collect.Maps;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.order.model.OrderFinishInfo;

/**
 * Created by cuiwentao
 * on 16/3/23
 */
@Repository
public class OrderFinishInfoDao extends MyBatisDao<OrderFinishInfo> {


    /**
     * 查询出没有结算的流水
     */
    public Paging<OrderFinishInfo> findNeedSettlements( Integer offset, Integer limit) {
        Map<String,Object> map = Maps.newHashMap();
        Long count = getSqlSession().selectOne(sqlId("countNeedSettlement"),map);
        if(count == 0) {
            return new Paging<OrderFinishInfo>(0L, Collections.<OrderFinishInfo>emptyList());
        }
        map.put("offset",offset);
        map.put("limit",limit);
        List<OrderFinishInfo> channels = getSqlSession().selectList(sqlId("pagingNeedSettlement"), map);
        return new Paging<OrderFinishInfo>(count, channels);
    }

    public Boolean updateSettlemented(Long id){
        return getSqlSession().update(sqlId("updateSettlemented"),id)>0;
    }


}
