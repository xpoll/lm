package cn.blmdz.rabbit.user.impl.dao;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

import java.util.List;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.rabbit.user.impl.dao.OperatorRoleDao;
import cn.blmdz.rabbit.user.model.OperatorRole;

public class OperatorRoleDaoTest extends BaseDaoTest {

    @Autowired
    private OperatorRoleDao operatorRoleDao;

    private OperatorRole mock(String appKey, Integer status) {
        //OperatorRole
        OperatorRole role = new OperatorRole();
        role.setName("name");
        role.setDesc("desc");
        role.setAppKey(appKey);
        role.setStatus(status);
        role.setAllowJson("[\"k1\", \"k2\"]");
        role.setExtraJson("{\"businessId\":1}");
        return role;
    }

    @Test
    public void testFindById() {
        OperatorRole role = mock("key1", 1);
        operatorRoleDao.create(role);

        OperatorRole actual = operatorRoleDao.findById(role.getId());
        assertNotNull(actual);

        OperatorRole role2 = mock("key2", 2);
        operatorRoleDao.create(role2);

        List<OperatorRole> founds = operatorRoleDao.findByIds(Lists.newArrayList(role.getId(), role2.getId()));
        assertEquals(2, founds.size());
        List<Long> actualIds = Lists.newArrayList(founds.get(0).getId(), founds.get(1).getId());
        assertThat(actualIds, containsInAnyOrder(role2.getId(), role.getId()));
    }

    @Test
    public void testUpdate() {
        OperatorRole role = mock("toUpdate", 1);
        operatorRoleDao.create(role);

        OperatorRole toUpdate = new OperatorRole();
        toUpdate.setId(role.getId());
        toUpdate.setName("_name");
        toUpdate.setDesc("_desc");
        toUpdate.setAppKey("_app_key");
        toUpdate.setAllowJson("[\"_changed\"]");
        toUpdate.setExtraJson("{\"changed\":1}");

        operatorRoleDao.update(toUpdate);

        OperatorRole actual = operatorRoleDao.findById(role.getId());
        assertEquals(toUpdate.getName(), actual.getName());
        assertEquals(toUpdate.getDesc(), actual.getDesc());
        assertEquals(toUpdate.getAppKey(), actual.getAppKey());
        assertEquals(toUpdate.getAllowJson(), actual.getAllowJson());
        assertEquals(toUpdate.getExtraJson(), actual.getExtraJson());
    }

    @Test
    public void testDelete() {
        OperatorRole role = mock("key1", 1);
        operatorRoleDao.create(role);
        assertNotNull(operatorRoleDao.findById(role.getId()));

        operatorRoleDao.delete(role.getId());
        assertNull(operatorRoleDao.findById(role.getId()));
    }

    @Test
    public void testPaging() {
        List<OperatorRole> roles = Lists.newArrayList(
                mock("key1", 1),
                mock("key2", 1),
                mock("key1", 2),
                mock("key2", 2),
                mock("key1", 1)
        );
        for (OperatorRole role : roles) {
            operatorRoleDao.create(role);
        }
        OperatorRole criteria = new OperatorRole();
        criteria.setAppKey("key1");
        Paging<OperatorRole> result = operatorRoleDao.paging(0, 10, criteria);
        assertThat(result.getTotal(), is(3L));
        List<Long> ids = Lists.newArrayList();
        for (OperatorRole o : result.getData()) {
            ids.add(o.getId());
        }
        assertThat(ids, containsInAnyOrder(roles.get(0).getId(), roles.get(2).getId(), roles.get(4).getId()));


        criteria.setStatus(1);
        result = operatorRoleDao.paging(0, 10, criteria);
        assertThat(result.getTotal(), is(2L));
        assertThat(
                Lists.newArrayList(result.getData().get(0).getId(), result.getData().get(1).getId()),
                containsInAnyOrder(roles.get(0).getId(), roles.get(4).getId())
        );
    }

    @Test
    public void testFindByStatus() {
        List<OperatorRole> roles = Lists.newArrayList(
                mock("key1", 1),
                mock("key2", 1),
                mock("key1", 2),
                mock("key2", 2),
                mock("key1", 1)
        );
        for (OperatorRole role : roles) {
            operatorRoleDao.create(role);
        }

        List<OperatorRole> result = operatorRoleDao.findByStatus("key1", 1);
        assertThat(result.size(), is(2));
        assertThat(
                Lists.newArrayList(result.get(0).getId(), result.get(1).getId()),
                containsInAnyOrder(roles.get(0).getId(), roles.get(4).getId())
        );
    }
}
