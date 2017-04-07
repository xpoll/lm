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
import cn.blmdz.rabbit.user.impl.dao.MainSellerRoleDao;
import cn.blmdz.rabbit.user.model.MainSellerRole;

public class MainSellerRoleDaoTest extends BaseDaoTest {

    @Autowired
    private MainSellerRoleDao mainSellerRoleDao;

    private MainSellerRole mock(Integer status) {
        //MainSellerRole
        MainSellerRole role = new MainSellerRole();
        role.setName("name");
        role.setDesc("desc");
        role.setStatus(status);
        role.setAllowJson("[\"k1\", \"k2\"]");
        role.setExtraJson("{\"businessId\":1}");
        return role;
    }

    @Test
    public void testFindById() {
        MainSellerRole role = mock(1);
        mainSellerRoleDao.create(role);

        MainSellerRole actual = mainSellerRoleDao.findById(role.getId());
        assertNotNull(actual);

        MainSellerRole role2 = mock(2);
        mainSellerRoleDao.create(role2);

        List<MainSellerRole> founds = mainSellerRoleDao.findByIds(Lists.newArrayList(role.getId(), role2.getId()));
        assertEquals(2, founds.size());
        List<Long> actualIds = Lists.newArrayList(founds.get(0).getId(), founds.get(1).getId());
        assertThat(actualIds, containsInAnyOrder(role2.getId(), role.getId()));
    }

    @Test
    public void testUpdate() {
        MainSellerRole role = mock(1);
        mainSellerRoleDao.create(role);

        MainSellerRole toUpdate = new MainSellerRole();
        toUpdate.setId(role.getId());
        toUpdate.setName("_name");
        toUpdate.setDesc("_desc");
        toUpdate.setAllowJson("[\"_changed\"]");
        toUpdate.setExtraJson("{\"changed\":1}");

        mainSellerRoleDao.update(toUpdate);

        MainSellerRole actual = mainSellerRoleDao.findById(role.getId());
        assertEquals(toUpdate.getName(), actual.getName());
        assertEquals(toUpdate.getDesc(), actual.getDesc());
        assertEquals(toUpdate.getAllowJson(), actual.getAllowJson());
        assertEquals(toUpdate.getExtraJson(), actual.getExtraJson());
    }

    @Test
    public void testDelete() {
        MainSellerRole role = mock(1);
        mainSellerRoleDao.create(role);
        assertNotNull(mainSellerRoleDao.findById(role.getId()));

        mainSellerRoleDao.delete(role.getId());
        assertNull(mainSellerRoleDao.findById(role.getId()));
    }

    @Test
    public void testPaging() {
        List<MainSellerRole> roles = Lists.newArrayList(
                mock(1),
                mock(1),
                mock(2),
                mock(2),
                mock(2)
        );
        for (MainSellerRole role : roles) {
            mainSellerRoleDao.create(role);
        }
        MainSellerRole criteria = new MainSellerRole();
        Paging<MainSellerRole> result = mainSellerRoleDao.paging(0, 10, criteria);
        assertThat(result.getTotal(), is(5L));
        List<Long> ids = Lists.newArrayList();
        for (MainSellerRole o : result.getData()) {
            ids.add(o.getId());
        }
        assertThat(ids, containsInAnyOrder(
                roles.get(0).getId(),
                roles.get(1).getId(),
                roles.get(2).getId(),
                roles.get(3).getId(),
                roles.get(4).getId()
        ));


        criteria.setStatus(1);
        result = mainSellerRoleDao.paging(0, 10, criteria);
        assertThat(result.getTotal(), is(2L));
        assertThat(
                Lists.newArrayList(
                        result.getData().get(0).getId(),
                        result.getData().get(1).getId()
                ),
                containsInAnyOrder(
                        roles.get(0).getId(),
                        roles.get(1).getId()
                )
        );
    }

    @Test
    public void testFindByStatus() {
        List<MainSellerRole> roles = Lists.newArrayList(
                mock(1),
                mock(1),
                mock(2)
        );
        for (MainSellerRole role : roles) {
            mainSellerRoleDao.create(role);
        }

        List<MainSellerRole> result = mainSellerRoleDao.findByStatus(1);
        assertThat(result.size(), is(2));
        assertThat(
                Lists.newArrayList(result.get(0).getId(), result.get(1).getId()),
                containsInAnyOrder(roles.get(0).getId(), roles.get(1).getId())
        );
    }
}
