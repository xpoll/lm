package cn.blmdz.wolf.item.impl.dao;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Repository;

import com.google.common.base.MoreObjects;
import com.google.common.collect.ImmutableMap;

import cn.blmdz.home.common.mysql.dao.MyBatisDao;
import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.item.model.Item;

@Repository
public class ItemDao extends MyBatisDao<Item> {
	public List<Item> findByShopIdAndCode(Long shopId, String itemCode) {
		return getSqlSession().selectList(sqlId("findByCode"), ImmutableMap.of("shopId", shopId, "itemCode", itemCode));
	}

	public List<Item> findByItemCode(String itemCode) {
		return getSqlSession().selectList(sqlId("findByCode"), ImmutableMap.of("itemCode", itemCode));
	}

	public void updateStatus(Long itemId, Integer status) {
		getSqlSession().update(sqlId("updateStatus"), ImmutableMap.of("id", itemId, "status", status));
	}

	public boolean updateStatusByShopIdAndItemId(Long shopId, Long itemId, Integer status) {
		return getSqlSession().update(sqlId("updateStatusByShopIdAndItemId"),
				ImmutableMap.of("id", itemId, "status", status, "shopId", shopId)) == 1;
	}

	public void updateItemInfoMd5(Long itemId, String itemInfoMd5) {
		getSqlSession().update(sqlId("updateItemInfoMd5"), ImmutableMap.of("id", itemId, "itemInfoMd5", itemInfoMd5));
	}

	public void batchUpdateStatus(List<Long> ids, Integer status) {
		getSqlSession().update(sqlId("batchUpdateStatus"), ImmutableMap.of("ids", ids, "status", status));
	}

	public boolean batchUpdateStatusByShopIdAndIds(Long shopId, List<Long> ids, Integer status) {
		return getSqlSession().update(sqlId("batchUpdateStatusByShopIdAndIds"),
				ImmutableMap.of("shopId", shopId, "ids", ids, "status", status)) == ids.size();
	}

	public void batchUpdateStatusByShopId(Long shopId, Integer status) {
		getSqlSession().update(sqlId("batchUpdateStatusByShopId"), ImmutableMap.of("shopId", shopId, "status", status));
	}

	public void updateTags(Long itemId, Map<String, String> tags, String itemInfoMd5) {
		getSqlSession().update(sqlId("updateTags"), ImmutableMap.of("itemId", itemId, "tagsJson",
				JsonMapper.JSON_NON_DEFAULT_MAPPER.toJson(tags), "itemInfoMd5", itemInfoMd5));
	}

	public Long maxId() {
		Long id = (Long) getSqlSession().selectOne(sqlId("maxId"));
		return (Long) MoreObjects.firstNonNull(id, Long.valueOf(0L));
	}

	public List<Item> listSince(Long lastId, String since, int limit) {
		return getSqlSession().selectList(sqlId("listSince"),
				ImmutableMap.of("lastId", lastId, "limit", Integer.valueOf(limit), "since", since));
	}

	public void updateSaleQuantity(Long itemId, Integer delta) {
		getSqlSession().update(sqlId("updateSaleQuantity"), ImmutableMap.of("itemId", itemId, "quantity", delta));
	}

	public long countOnShelfByShopId(Long shopId) {
		return ((Long) getSqlSession().selectOne(sqlId("countOnShelfByShopId"), shopId)).longValue();
	}
}