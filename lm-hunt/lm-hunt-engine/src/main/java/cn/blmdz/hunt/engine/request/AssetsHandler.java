package cn.blmdz.hunt.engine.request;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import com.google.common.collect.Iterables;

import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.engine.Setting;
import cn.blmdz.hunt.engine.utils.FileLoader;
import cn.blmdz.hunt.engine.utils.FileLoaderHelper;
import cn.blmdz.hunt.engine.utils.MimeTypes;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class AssetsHandler {
	@Autowired
	private Setting setting;
	@Autowired
	private FileLoaderHelper fileLoaderHelper;

	public boolean handle(String path, HttpServletResponse response) {
		String lastPath = Iterables.getLast(Splitters.SLASH.split(path));
		List<String> fileInfo = Splitters.DOT.splitToList(lastPath);
		if (fileInfo.size() == 1)
			return false;

		response.setContentType(MimeTypes.getType(Iterables.getLast(fileInfo)));

		String realPath = setting.getRootPath() + path;
		FileLoader.Resp resp = fileLoaderHelper.load(realPath);
		if (resp.isNotFound()) {
			if (log.isDebugEnabled())
				log.debug("Assets not found, path: [{}]", path);
			response.setStatus(HttpStatus.NOT_FOUND.value());
			return true;
		}
		response.setContentLength(resp.getContext().length);

		try {
			response.getOutputStream().write(resp.getContext());
		} catch (IOException e) {
		}

		return true;
	}
}