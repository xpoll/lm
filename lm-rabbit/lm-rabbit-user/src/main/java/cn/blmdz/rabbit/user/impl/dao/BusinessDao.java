package cn.blmdz.rabbit.user.impl.dao;

import java.util.List;
import java.util.Map;

import org.apache.commons.collections.map.HashedMap;
import org.springframework.stereotype.Repository;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.rabbit.user.model.Business;

/**
 * Created by liushaofei on 16/7/29.
 */
@Repository
public class BusinessDao extends MyBatisDao<Business>{

    public Response<Paging<Business>> select(Business business, Integer pageIndex, Integer pageSize){
        Map<String, Object> map = new HashedMap();
        map.put("business", business);
        map.put("pageIndex", pageIndex);
        map.put("pageSize", pageSize);
        List<Business> list = getSqlSession().selectList(sqlId("select"), map);
        return Response.ok(new Paging<>(new Long(list.size()), list));
    }

    public Integer insert(Business business){
        return getSqlSession().insert(sqlId("insert"), business);
    }

    public Integer delete(Business business){
        Map<String, Object> map = new HashedMap();
        map.put("business", business);
        return getSqlSession().delete(sqlId("delete"), map);
    }

    /**
     * 修改
     * 注意事项: 最小包含键值对,key = "business" ,value的类型Business,要查询的值;
     * key = "change", value的类型Business,改变后的值
     * @param map
     * @return
     */
    public Integer updateOne(Map<String, Object> map){
        return getSqlSession().update(sqlId("update"), map);
    }

    public List<Business> findByIds(List<Long> ids){
        return getSqlSession().selectList(sqlId("findByIds"), ids);
    }
}
