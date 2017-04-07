package cn.blmdz.rabbit.user.impl.dao;


import org.apache.commons.collections.map.HashedMap;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import cn.blmdz.rabbit.user.impl.dao.BusinessDao;
import cn.blmdz.rabbit.user.model.Business;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertNotNull;


/**
 * Created by liushaofei on 16/8/9.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = DaoConfiguration.class)
@Transactional
@Rollback
@ActiveProfiles("test")
public class BusinessDaoTest {
    @Autowired
    BusinessDao businessDao;

    public Business createOne(long id){
        Business business = new Business();
        business.setId(id);
        return business;
    }

    /**
     * 由于Business对象的参数比较多,id是自增的,所以只测试type和businessName
     * 在改测试中将insert,select,delete全部进行了测试
     */
    @Test
    public void select(){
        Business business = new Business();
        business.setType(10);
        businessDao.insert(business);
        assertNotNull(businessDao.select(business, null, null).getResult().getData().get(0));
        businessDao.delete(business);
        Assert.assertTrue(businessDao.select(business, null, null).getResult().getData().size() <=0);
        business.setBusinessName("test");
        businessDao.insert(business);
        assertNotNull(businessDao.select(business, null, null).getResult().getData().get(0));
        businessDao.delete(business);
        Assert.assertTrue(businessDao.select(business, null, null).getResult().getData().size() <= 0);
    }

    @Test
    public void updateOne(){
        Business business = new Business();
        business.setBusinessName("test");
        businessDao.insert(business);
        Map<String, Object> map = new HashedMap();
        map.put("business", business);
        Business change = new Business();
        change.setBusinessName("test2");
        map.put("change", change);
        businessDao.updateOne(map);
        Assert.assertNotNull(businessDao.select(change, null, null).getResult().getData().get(0));
        businessDao.delete(change);
        Assert.assertTrue(businessDao.select(business, null, null).getResult().getData().size() <=0);
    }

    @Test
    public void findByIds(){
        List<Long> ids = new LinkedList<>();
        Business business = new Business();
        business.setBusinessName("test1");
        businessDao.insert(business);
        ids.add(businessDao.select(business,null, null).getResult().getData().get(0).getId());
        business.setBusinessName("test2");
        businessDao.insert(business);
        ids.add(businessDao.select(business,null, null).getResult().getData().get(0).getId());
        business.setBusinessName("test3");
        businessDao.insert(business);
        ids.add(businessDao.select(business,null, null).getResult().getData().get(0).getId());
        business.setBusinessName("test4");
        businessDao.insert(business);
        ids.add(businessDao.select(business,null, null).getResult().getData().get(0).getId());
        Assert.assertTrue(businessDao.findByIds(ids).size() == 4);
    }

}
