package io.terminus.galaxy.user.impl.dao;

import com.google.common.collect.ImmutableMap;
import io.terminus.common.mysql.dao.MyBatisDao;
import io.terminus.galaxy.user.model.Notice;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Created Date : 16/7/29
 * Author : wujianwei
 */
@Repository
public class NoticeDao extends MyBatisDao<Notice> {

    public Boolean updateStatus(Long noticeId, Integer status) {
        return  this.getSqlSession().update(sqlId("updateStatus"), ImmutableMap.of("id",noticeId,"status",status))==1;

    }
    public Notice findById(Long noticeId) {
        return  this.getSqlSession().selectOne(sqlId("findById"), ImmutableMap.of("id",noticeId));
    }

    public Boolean updateNotice(Notice notice) {
        return  this.getSqlSession().update(sqlId("updateNoticeById"), notice)==1;

    }
    public Boolean delete(Long noticeId){
        return this.getSqlSession().delete(sqlId("deleteNotice"),noticeId)==1;
    }


    public List<Notice> findAll(){
        return this.getSqlSession().selectList(sqlId("findAll"));
    }

}
